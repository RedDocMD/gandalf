import subprocess as sp
import tempfile
import json


def run_sat(filename, seqlen, dump=None):
    dump_str = f' -dump_json {dump}' if dump else ''
    with tempfile.NamedTemporaryFile(suffix='.tcl', mode='w') as f:
        print(f"read_verilog {filename}", file=f)
        print("prep -top puzzle", file=f)
        print(f"sat -seq {seqlen} -set-init-zero -set-at {seqlen} success 1{dump_str}", file=f)
        f.flush()
        return sp.check_output(f"yosys -s {f.name}", shell=True).decode()


def check_sat(filename, seqlen):
    output = run_sat(filename, seqlen)
    return 'no model found' not in output


def get_min_seq_len(filename):
    # Estimated answer should be 121-ish
    lo = 80
    hi = 200
    ans = None
    while lo <= hi:
        mid = lo + (hi - lo) // 2
        if check_sat(filename, mid):
            ans = mid
            hi = mid - 1
        else:
            lo = mid + 1
    return ans


def parse_waveform(wave):
    state = None
    bits = []
    for chr in wave:
        if chr == '1' or chr == '0':
            bits.append(chr)
            state = chr
        elif chr == '.':
            bits.append(state)
        else:
            bits.append('x')
            state = 'x'
    return bits


filename = 'netlist/puzzle_flattened_success.v'
min_seq = get_min_seq_len(filename)
print(f"Minimum sequence to get success is {min_seq}")
with tempfile.NamedTemporaryFile(suffix=".json", mode='w') as outfile:
    run_sat(filename, min_seq, outfile.name)
    with open(outfile.name) as signal_file:
        waves = json.load(signal_file)
        signals = waves['signal']
        for signal in signals:
            if signal['name'] == 'I':
                bits = parse_waveform(signal['wave'])
                bitstr = ''.join(bits)
                print(f"I = {bitstr}")
