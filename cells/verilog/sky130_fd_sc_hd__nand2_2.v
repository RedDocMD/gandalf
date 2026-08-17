module sky130_fd_sc_hd__nand2_2 (
    input  A,
    input  B,
    output Y
);

    assign Y = ~(A & B);

endmodule
