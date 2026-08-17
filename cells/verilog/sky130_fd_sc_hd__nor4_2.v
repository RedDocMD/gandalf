module sky130_fd_sc_hd__nor4_2 (
    input  A,
    input  B,
    input  C,
    input  D,
    output Y
);

    assign Y = ~(A | B | C | D);

endmodule
