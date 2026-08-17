module sky130_fd_sc_hd__nor4b_2 (
    input  A,
    input  B,
    input  C,
    input  D_N,
    output Y
);

    assign Y = ~(A | B | C | ~D_N);

endmodule
