module sky130_fd_sc_hd__nand3_2 (
    input  A,
    input  B,
    input  C,
    output Y
);

    assign Y = ~(A & B & C);

endmodule
