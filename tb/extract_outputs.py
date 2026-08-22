import subprocess as sp
import json


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

sp.call("yosys -s tcl/extract.tcl", shell=True)
with open("/tmp/signals.json") as signal_file:
    waves = json.load(signal_file)
    signals = waves['signal']
    for signal in signals:
        if signal['name'] == 'I':
            bits = parse_waveform(signal['wave'])
            bitstr = ''.join(bits)
            print(f"I = {bitstr}")