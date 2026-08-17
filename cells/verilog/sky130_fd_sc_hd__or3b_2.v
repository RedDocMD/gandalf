module sky130_fd_sc_hd__or3b_2 (
    input  A,
    input  B,
    input  C_N,
    output X
);

    assign X = (A | B | ~C_N);

endmodule
