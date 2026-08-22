import argparse
import math

GRID_SIZE = 11
CELL = 40
MARGIN = 10


def star_points(cx, cy, outer_r, inner_r):
    points = []
    for i in range(10):
        angle = -math.pi / 2 + i * math.pi / 5
        r = outer_r if i % 2 == 0 else inner_r
        x = cx + r * math.cos(angle)
        y = cy + r * math.sin(angle)
        points.append(f"{x:.2f},{y:.2f}")
    return " ".join(points)


def render_svg(bitstring):
    side = GRID_SIZE * CELL
    width = height = side + 2 * MARGIN
    lines = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" '
        f'viewBox="0 0 {width} {height}">',
        f'<rect x="0" y="0" width="{width}" height="{height}" fill="white"/>',
    ]

    for i in range(GRID_SIZE + 1):
        x = MARGIN + i * CELL
        lines.append(f'<line x1="{x}" y1="{MARGIN}" x2="{x}" y2="{MARGIN + side}" '
                      f'stroke="black" stroke-width="1"/>')
        y = MARGIN + i * CELL
        lines.append(f'<line x1="{MARGIN}" y1="{y}" x2="{MARGIN + side}" y2="{y}" '
                      f'stroke="black" stroke-width="1"/>')

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
    args = parser.parse_args()

    if len(args.bitstring) != GRID_SIZE * GRID_SIZE:
        parser.error(f"bitstring must be {GRID_SIZE * GRID_SIZE} characters, got {len(args.bitstring)}")
    if any(c not in '01' for c in args.bitstring):
        parser.error("bitstring must contain only 0s and 1s")

    svg = render_svg(args.bitstring)
    with open(args.output, 'w') as f:
        f.write(svg)


if __name__ == "__main__":
    main()
