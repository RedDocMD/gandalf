import argparse
import json
import math
from pathlib import Path

GRID_SIZE = 11
CELL = 40
MARGIN = 10
THIN_WIDTH = 1
THICK_WIDTH = 4
SHADE_COLOR = "#3366ff"
SHADE_OPACITY = 0.2


def star_points(cx, cy, outer_r, inner_r):
    points = []
    for i in range(10):
        angle = -math.pi / 2 + i * math.pi / 5
        r = outer_r if i % 2 == 0 else inner_r
        x = cx + r * math.cos(angle)
        y = cy + r * math.sin(angle)
        points.append(f"{x:.2f},{y:.2f}")
    return " ".join(points)


def load_regions(regions_path):
    """regions JSON: name -> list of bit positions."""
    return json.loads(regions_path.read_text())


def region_of_map(regions):
    """regions: name -> list of bit positions. Returns pos -> name."""
    region_of = {}
    for name, positions in regions.items():
        for pos in positions:
            region_of[pos] = name
    return region_of


def render_svg(bitstring, region_of=None, shade_positions=None):
    side = GRID_SIZE * CELL
    width = height = side + 2 * MARGIN
    lines = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" '
        f'viewBox="0 0 {width} {height}">',
        f'<rect x="0" y="0" width="{width}" height="{height}" fill="white"/>',
    ]

    if shade_positions:
        for idx in shade_positions:
            row, col = divmod(idx, GRID_SIZE)
            x = MARGIN + col * CELL
            y = MARGIN + row * CELL
            lines.append(f'<rect x="{x}" y="{y}" width="{CELL}" height="{CELL}" '
                          f'fill="{SHADE_COLOR}" fill-opacity="{SHADE_OPACITY}"/>')

    for i in range(GRID_SIZE + 1):
        x = MARGIN + i * CELL
        lines.append(f'<line x1="{x}" y1="{MARGIN}" x2="{x}" y2="{MARGIN + side}" '
                      f'stroke="black" stroke-width="{THIN_WIDTH}"/>')
        y = MARGIN + i * CELL
        lines.append(f'<line x1="{MARGIN}" y1="{y}" x2="{MARGIN + side}" y2="{y}" '
                      f'stroke="black" stroke-width="{THIN_WIDTH}"/>')

    if region_of:
        # Thick outer border around the whole grid.
        lines.append(f'<rect x="{MARGIN}" y="{MARGIN}" width="{side}" height="{side}" '
                      f'fill="none" stroke="black" stroke-width="{THICK_WIDTH}"/>')

        # Thick borders wherever two orthogonally-adjacent cells belong to
        # different regions.
        for row in range(GRID_SIZE):
            for col in range(GRID_SIZE):
                pos = row * GRID_SIZE + col
                here = region_of.get(pos)

                if col + 1 < GRID_SIZE and region_of.get(pos + 1) != here:
                    x = MARGIN + (col + 1) * CELL
                    y1 = MARGIN + row * CELL
                    y2 = y1 + CELL
                    lines.append(f'<line x1="{x}" y1="{y1}" x2="{x}" y2="{y2}" '
                                  f'stroke="black" stroke-width="{THICK_WIDTH}"/>')

                if row + 1 < GRID_SIZE and region_of.get(pos + GRID_SIZE) != here:
                    y = MARGIN + (row + 1) * CELL
                    x1 = MARGIN + col * CELL
                    x2 = x1 + CELL
                    lines.append(f'<line x1="{x1}" y1="{y}" x2="{x2}" y2="{y}" '
                                  f'stroke="black" stroke-width="{THICK_WIDTH}"/>')

    outer_r = CELL * 0.4
    inner_r = outer_r * 0.4
    for idx, bit in enumerate(bitstring):
        if bit != '1':
            continue
        row, col = divmod(idx, GRID_SIZE)
        cx = MARGIN + col * CELL + CELL / 2
        cy = MARGIN + row * CELL + CELL / 2
        lines.append(f'<polygon points="{star_points(cx, cy, outer_r, inner_r)}" fill="black"/>')

    lines.append('</svg>')
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(description="Render an 11x11 not-touch-star grid as SVG")
    parser.add_argument("bitstring", help="121-character string of 0/1, row-major")
    parser.add_argument("output", help="path to write the output SVG")
    parser.add_argument("--regions", type=Path, default=None,
                         help="JSON file mapping region name -> list of bit positions "
                              "(e.g. from find_grid_regions.py --json); draws a thick "
                              "border around each region")
    parser.add_argument("--shade", default=None,
                         help="comma-separated list of region names (signal names from "
                              "--regions) to lightly shade blue")
    args = parser.parse_args()

    if len(args.bitstring) != GRID_SIZE * GRID_SIZE:
        parser.error(f"bitstring must be {GRID_SIZE * GRID_SIZE} characters, got {len(args.bitstring)}")
    if any(c not in '01' for c in args.bitstring):
        parser.error("bitstring must contain only 0s and 1s")

    if args.shade and not args.regions:
        parser.error("--shade requires --regions")

    region_of = None
    shade_positions = None
    if args.regions:
        regions = load_regions(args.regions)
        region_of = region_of_map(regions)
        if args.shade:
            shade_names = {name.strip() for name in args.shade.split(",") if name.strip()}
            unknown = shade_names - regions.keys()
            if unknown:
                parser.error(f"unknown region name(s) in --shade: {sorted(unknown)}")
            shade_positions = {pos for pos, name in region_of.items() if name in shade_names}

    svg = render_svg(args.bitstring, region_of, shade_positions)
    with open(args.output, 'w') as f:
        f.write(svg)


if __name__ == "__main__":
    main()
