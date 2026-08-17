module sky130_fd_sc_hd__and4b_2 (
    input  A_N,
    input  B,
    input  C,
    input  D,
    output X
);

    assign X = (~A_N & B & C & D);

endmodule
