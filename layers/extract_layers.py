#!/usr/bin/env python3
"""Turn a KLayout .lyt/.lyp/.map technology triple into a JSON connectivity summary."""

import argparse
import json
import re
import xml.etree.ElementTree as ET

LD_RE = re.compile(r"(\d+)/(\d+)")


def parse_lyp(path):
    tree = ET.parse(path)
    layers = []
    lookup = {}
    for props in tree.getroot().iter("properties"):
        name_el = props.find("name")
        source_el = props.find("source")
        if name_el is None or name_el.text is None:
            continue
        name = name_el.text.split(" - ")[0].strip()

        m = None
        if source_el is not None and source_el.text:
            m = LD_RE.search(source_el.text)
        if m is None:
            m = LD_RE.search(name_el.text)
        if m is None:
            continue
        layer, datatype = int(m.group(1)), int(m.group(2))

        layers.append({"name": name, "layer": layer, "datatype": datatype})

        base_name = name.split(".")[0]
        lookup.setdefault((layer, datatype), base_name)

    return layers, lookup


def parse_map(path):
    entries = []
    with open(path) as f:
        for line in f:
            parts = line.split()
            if len(parts) != 4:
                continue
            layer_name, purposes, layer, datatype = parts
            entries.append(
                {
                    "name": layer_name,
                    "purposes": purposes,
                    "layer": int(layer),
                    "datatype": int(datatype),
                }
            )
    return entries


def resolve_symbol(expr, lookup):
    m = LD_RE.search(expr)
    if m is None:
        return None
    layer, datatype = int(m.group(1)), int(m.group(2))
    return lookup.get((layer, datatype))


def parse_lyt(path, lookup):
    tree = ET.parse(path)
    connectivity = tree.getroot().find("connectivity")
    if connectivity is None:
        return [], []

    symbols = {}
    for el in connectivity.findall("symbols"):
        if not el.text or "=" not in el.text:
            continue
        sym_name, expr = el.text.split("=", 1)
        sym_name = sym_name.strip()
        expr = expr.strip().strip("'")
        symbols[sym_name] = resolve_symbol(expr, lookup) or sym_name

    direct_connections = []
    seen_direct = set()
    cross_connections = []
    for el in connectivity.findall("connection"):
        if not el.text:
            continue
        parts = [p.strip() for p in el.text.split(",")]
        if len(parts) != 3:
            continue
        a_sym, v_sym, b_sym = parts
        a_name = symbols.get(a_sym, a_sym)
        v_name = symbols.get(v_sym, v_sym)
        b_name = symbols.get(b_sym, b_sym)

        for layer_name in (a_name, b_name):
            if layer_name not in seen_direct:
                seen_direct.add(layer_name)
                direct_connections.append({"layer": layer_name})

        cross_connections.append({"layers": [a_name, b_name], "via": v_name})

    return direct_connections, cross_connections


def main():
    parser = argparse.ArgumentParser(
        description="Build a JSON layer/connectivity summary from KLayout tech files."
    )
    parser.add_argument("lyt", help="KLayout .lyt technology file")
    parser.add_argument("lyp", help="KLayout .lyp layer-properties file")
    parser.add_argument("map", help="LEF/DEF .map layer-mapping file")
    parser.add_argument("-o", "--output", help="output JSON file (default: stdout)")
    args = parser.parse_args()

    layers, lookup = parse_lyp(args.lyp)
    parse_map(args.map)  # required input; validated by loading successfully
    direct_connections, cross_connections = parse_lyt(args.lyt, lookup)

    result = {
        "layers": layers,
        "direct_connections": direct_connections,
        "cross_connections": cross_connections,
    }

    text = json.dumps(result, indent=2)
    if args.output:
        with open(args.output, "w") as f:
            f.write(text + "\n")
    else:
        print(text)


if __name__ == "__main__":
    main()
