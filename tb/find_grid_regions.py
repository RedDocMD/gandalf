#!/usr/bin/env python3
"""Reconstruct grid groupings (columns, then whatever's left) from the
per-bit local-counter signals found by scan_local_counters.py.

The 123-bit input is an 11x11 grid in row-major order (positions 0..120;
120 and 122 are the two trailing don't-care bits). For every pair of grid
positions we look at which `puz` signals turn 1 one cycle after both of
them individually (delta 1), the same computation find_common_signals.py
does. A handful of signals are common no matter which two positions you
pick (pure pipeline noise); a handful more are common to an entire column.
Once both of those are stripped out, whatever common signal remains for a
pair should identify a shared structure (e.g. a row) -- we collect the
positions sharing each such signal and print them out.
"""
import argparse
import json
import shutil
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import find_common_signals as fcs  # noqa: E402

GRID_SIZE = 11
NUM_POSITIONS = GRID_SIZE * GRID_SIZE  # 121, positions 0..120
DELTA = 1

GLOBAL_REMOVE = {"_0302_", "_0393_", "net175", "net374", "net662"}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--json", type=Path, default=None,
                         help="write the residual signal -> bit positions mapping as JSON to this path")
    args = parser.parse_args()

    tmp_root = Path(tempfile.mkdtemp(prefix="find_grid_regions_"))
    try:
        print(f"Scanning positions 0..{NUM_POSITIONS - 1} (caching results in {tmp_root})...")
        scan_data = {}
        for position in range(NUM_POSITIONS):
            json_path = tmp_root / f"scan_{position}.json"
            scan_data[position] = fcs.run_scan(position, json_path)
            if (position + 1) % 11 == 0 or position == NUM_POSITIONS - 1:
                print(f"  scanned {position + 1}/{NUM_POSITIONS}")

        # --- column commonality --------------------------------------
        column_remove = set()
        print("\nColumn common signals (delta 1, global noise removed):")
        for c in range(GRID_SIZE):
            col_positions = [r * GRID_SIZE + c for r in range(GRID_SIZE)]
            common = fcs.common_signals([scan_data[p] for p in col_positions], DELTA) - GLOBAL_REMOVE
            print(f"  column {c} {col_positions}: {sorted(common)}")
            if len(common) != 1:
                print(f"    WARNING: expected exactly 1 column signal, got {len(common)}")
            column_remove |= common

        remove_set = GLOBAL_REMOVE | column_remove
        print(f"\nTotal removed signals (global + column): {sorted(remove_set)}\n")

        # --- pairwise commonality --------------------------------------
        signal_positions = {}
        print(f"Checking all {NUM_POSITIONS * (NUM_POSITIONS - 1) // 2} pairs at delta {DELTA}...")
        for i in range(NUM_POSITIONS):
            for j in range(i + 1, NUM_POSITIONS):
                remaining = fcs.common_signals([scan_data[i], scan_data[j]], DELTA) - remove_set
                if len(remaining) > 1:
                    print(f"\nFLAG: pair ({i}, {j}) has {len(remaining)} common signals "
                          f"after removing global/column noise: {sorted(remaining)}")
                    sys.exit(1)
                elif len(remaining) == 1:
                    (sig,) = remaining
                    signal_positions.setdefault(sig, set()).update((i, j))

        print(f"\nNo pair had more than 1 residual common signal.\n")
        print(f"Residual common signals and their bit positions ({len(signal_positions)} signal(s)):\n")
        for sig in sorted(signal_positions):
            positions = sorted(signal_positions[sig])
            print(f"  {sig}: {positions}")

        if args.json:
            regions = {sig: sorted(positions) for sig, positions in signal_positions.items()}
            args.json.write_text(json.dumps(regions, indent=2))
            print(f"\nWrote {len(regions)} region(s) to {args.json}")
    finally:
        shutil.rmtree(tmp_root, ignore_errors=True)


if __name__ == "__main__":
    main()
