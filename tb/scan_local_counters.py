#!/usr/bin/env python3
"""Probe the puzzle netlist with a single input bit set and look for
registers/wires inside `puz` that latch high shortly after that bit
arrives -- candidates for per-region "local counters".
"""
import argparse
import json
import subprocess
import sys
from pathlib import Path

WIDTH = 123
REPO_ROOT = Path(__file__).resolve().parent.parent
I_BITS_PATH = REPO_ROOT / "tb" / "I_bits.txt"
VCD_PATH = REPO_ROOT / "tb" / "waves_probe.vcd"
SIM_CMD = (
    "iverilog -o sim.vvp tb/probe_tb.v netlist/puzzle_flattened_success_rst.v "
    "&& vvp sim.vvp && rm sim.vvp"
)


def build_bitstring(position):
    if not (0 <= position < WIDTH):
        raise ValueError(f"position must be in [0, {WIDTH}), got {position}")
    bits = ["0"] * WIDTH
    bits[position] = "1"
    return "".join(bits)


def run_simulation(quiet=False):
    result = subprocess.run(
        SIM_CMD, shell=True, cwd=REPO_ROOT,
        capture_output=quiet, text=True,
    )
    if result.returncode != 0:
        if quiet:
            sys.stderr.write(result.stdout or "")
            sys.stderr.write(result.stderr or "")
        sys.exit(f"simulation command failed with exit code {result.returncode}")


def parse_vcd(path):
    """Returns (var_names, transitions) where var_names[id] is a list of
    (scope_tuple, name, width) and transitions[id] is a chronological list
    of (time, value) for scalar (1-bit) signals."""
    var_names = {}
    transitions = {}
    scope_stack = []
    in_header = True
    current_time = None

    with open(path) as f:
        for raw_line in f:
            line = raw_line.strip()
            if not line:
                continue

            if in_header:
                if line.startswith("$scope"):
                    parts = line.split()
                    scope_stack.append(parts[2])
                elif line.startswith("$upscope"):
                    scope_stack.pop()
                elif line.startswith("$var"):
                    parts = line.split()
                    width = int(parts[2])
                    vid = parts[3]
                    name = parts[4]
                    var_names.setdefault(vid, []).append((tuple(scope_stack), name, width))
                elif line.startswith("$enddefinitions"):
                    in_header = False
                continue

            if line.startswith("#"):
                current_time = int(line[1:])
                continue

            if line.startswith("$"):
                continue

            c = line[0]
            if c in "01xzXZ":
                val = c.lower()
                vid = line[1:]
                transitions.setdefault(vid, []).append((current_time, val))
            # vector changes ('b...'/'r...') are ignored: we only care about
            # single-bit signals for the "0 -> 1 latch" property.

    return var_names, transitions


def puz_scoped_names(var_names):
    """id -> first name declared directly under a `puz` scope, for
    single-bit signals only."""
    result = {}
    for vid, entries in var_names.items():
        for scope, name, width in entries:
            if width == 1 and scope and scope[-1] == "puz":
                result.setdefault(vid, name)
    return result


def find_id_by_name(var_names, target_name):
    for vid, entries in var_names.items():
        for scope, name, width in entries:
            if name == target_name and scope and scope[-1] == "puz":
                return vid
    return None


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("position", type=int, help=f"index in [0, {WIDTH}) of the bit to set in I_bits.txt")
    parser.add_argument("--json", type=Path, default=None, help="write results (signal, cycle, delta) as JSON to this path")
    parser.add_argument("--quiet", action="store_true", help="suppress the human-readable table/status output")
    args = parser.parse_args()

    bitstring = build_bitstring(args.position)
    I_BITS_PATH.write_text(bitstring)
    if not args.quiet:
        print(f"Wrote {I_BITS_PATH.relative_to(REPO_ROOT)} with bit {args.position} set.")

    run_simulation(quiet=args.quiet)

    var_names, transitions = parse_vcd(VCD_PATH)

    clk_id = find_id_by_name(var_names, "clk")
    i_id = find_id_by_name(var_names, "I")
    if clk_id is None or i_id is None:
        sys.exit("could not find clk or I signal in waves_probe.vcd")

    posedges = sorted(t for t, v in transitions.get(clk_id, []) if v == "1")
    cycle_of_time = {t: idx + 1 for idx, t in enumerate(posedges)}

    i_events = transitions.get(i_id, [])
    i_blip_time = None
    for (t_prev, v_prev), (t_cur, v_cur) in zip(i_events, i_events[1:]):
        if v_prev == "0" and v_cur == "1":
            i_blip_time = t_cur
            break
    if i_blip_time is None:
        sys.exit("I never transitioned 0 -> 1 in the trace")
    i_blip_cycle = cycle_of_time.get(i_blip_time)

    candidates = puz_scoped_names(var_names)

    rows = []
    for vid, name in candidates.items():
        if vid in (i_id, clk_id):
            continue
        events = transitions.get(vid, [])
        if len(events) != 2:
            continue
        (t0, v0), (t1, v1) = events
        if v0 != "0" or v1 != "1":
            continue
        cycle = cycle_of_time.get(t1)
        if cycle is None or cycle < i_blip_cycle:
            continue
        rows.append((cycle, name, cycle - i_blip_cycle))

    rows.sort(key=lambda r: (r[0], r[1]))

    if args.json:
        payload = {
            "position": args.position,
            "i_blip_cycle": i_blip_cycle,
            "i_blip_time": i_blip_time,
            "signals": [
                {"signal": name, "cycle": cycle, "delta": delta}
                for cycle, name, delta in rows
            ],
        }
        args.json.write_text(json.dumps(payload, indent=2))
        if not args.quiet:
            print(f"Wrote {len(rows)} signal(s) to {args.json}")

    if args.quiet:
        return

    print(f"\nI blipped to 1 at cycle {i_blip_cycle} (t={i_blip_time})\n")

    if not rows:
        print("No qualifying signals found.")
        return

    name_w = max(len("Signal"), max(len(r[1]) for r in rows))
    cyc_w = max(len("Cycle turned 1"), len(str(rows[-1][0])))
    delta_w = len("Delta")

    header = f"{'Signal':<{name_w}}  {'Cycle turned 1':>{cyc_w}}  {'Delta':>{delta_w}}"
    print(header)
    print("-" * len(header))
    for cycle, name, delta in rows:
        print(f"{name:<{name_w}}  {cycle:>{cyc_w}}  {delta:>{delta_w}}")


if __name__ == "__main__":
    main()
