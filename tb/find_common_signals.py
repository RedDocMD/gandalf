#!/usr/bin/env python3
"""Wrapper around scan_local_counters.py: run it once per bit position in a
comma-separated list, then report which `puz` signals turn 1 at the same
requested delta (relative to when I blips) in *every* one of those runs.
"""
import argparse
import json
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
SCAN_SCRIPT = REPO_ROOT / "tb" / "scan_local_counters.py"


def run_scan(position, json_path):
    result = subprocess.run(
        [sys.executable, str(SCAN_SCRIPT), str(position), "--json", str(json_path), "--quiet"],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        sys.stderr.write(result.stdout)
        sys.stderr.write(result.stderr)
        sys.exit(f"scan_local_counters.py failed for position {position}")
    return json.loads(json_path.read_text())


def signals_at_delta(data, delta):
    """data: parsed JSON from scan_local_counters.py --json.
    Returns {signal_name: cycle} for signals whose delta matches."""
    return {entry["signal"]: entry["cycle"] for entry in data["signals"] if entry["delta"] == delta}


def common_signals(scan_data_list, delta):
    """scan_data_list: list of parsed JSON blobs (one per position).
    Returns the set of signal names present at `delta` in every one of them."""
    sets = [set(signals_at_delta(d, delta).keys()) for d in scan_data_list]
    return set.intersection(*sets) if sets else set()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bits", help="comma-separated list of bit positions, e.g. 0,12,60")
    parser.add_argument("delta", type=int, help="delta (cycle - I's blip cycle) to look for common signals at")
    args = parser.parse_args()

    positions = [int(b.strip()) for b in args.bits.split(",") if b.strip() != ""]
    if not positions:
        sys.exit("no bit positions given")

    per_position_signals = {}
    per_position_cycle = {}

    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)
        for position in positions:
            json_path = tmpdir / f"scan_{position}.json"
            data = run_scan(position, json_path)
            at_delta = signals_at_delta(data, args.delta)
            per_position_signals[position] = set(at_delta.keys())
            per_position_cycle[position] = at_delta
            print(f"position {position}: I blipped at cycle {data['i_blip_cycle']}, "
                  f"{len(at_delta)} signal(s) at delta {args.delta}")

    common = set.intersection(*per_position_signals.values())

    print(f"\nSignals at delta {args.delta} common to all {len(positions)} position(s): {len(common)}\n")

    if not common:
        return

    name_w = max(len("Signal"), max(len(name) for name in common))
    col_headers = [f"pos {p}" for p in positions]
    col_w = [max(len(h), 6) for h in col_headers]

    header = f"{'Signal':<{name_w}}  " + "  ".join(f"{h:>{w}}" for h, w in zip(col_headers, col_w))
    print(header)
    print("-" * len(header))
    for name in sorted(common):
        cells = [f"{per_position_cycle[p][name]:>{w}}" for p, w in zip(positions, col_w)]
        print(f"{name:<{name_w}}  " + "  ".join(cells))


if __name__ == "__main__":
    main()
