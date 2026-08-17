module sky130_fd_sc_hd__nor3_2 (
    input  A,
    input  B,
    input  C,
    output Y
);

    assign Y = ~(A | B | C);

endmodule
