module sky130_fd_sc_hd__and4bb_2 (
    input  A_N,
    input  B_N,
    input  C,
    input  D,
    output X
);

    assign X = (~A_N & ~B_N & C & D);

endmodule
