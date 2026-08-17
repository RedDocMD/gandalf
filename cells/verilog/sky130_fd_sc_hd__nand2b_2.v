module sky130_fd_sc_hd__nand2b_2 (
    input  A_N,
    input  B,
    output Y
);

    assign Y = ~(~A_N & B);

endmodule
