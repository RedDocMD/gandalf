module sky130_fd_sc_hd__nand3b_2 (
    input  A_N,
    input  B,
    input  C,
    output Y
);

    assign Y = ~(~A_N & B & C);

endmodule
