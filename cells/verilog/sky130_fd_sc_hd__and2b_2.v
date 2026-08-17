module sky130_fd_sc_hd__and2b_2 (
    input  A_N,
    input  B,
    output X
);

    assign X = (~A_N & B);

endmodule
