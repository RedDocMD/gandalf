module sky130_fd_sc_hd__nor3b_2 (
    input  A,
    input  B,
    input  C_N,
    output Y
);

    assign Y = ~(A | B | ~C_N);

endmodule
