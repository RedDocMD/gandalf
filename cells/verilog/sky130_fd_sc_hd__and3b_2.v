module sky130_fd_sc_hd__and3b_2 (
    input  A_N,
    input  B,
    input  C,
    output X
);

    assign X = (~A_N & B & C);

endmodule
