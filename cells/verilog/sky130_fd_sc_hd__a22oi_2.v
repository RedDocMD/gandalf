module sky130_fd_sc_hd__a22oi_2 (
    input  A1,
    input  A2,
    input  B1,
    input  B2,
    output Y
);

    assign Y = ~((A1 & A2) | (B1 & B2));

endmodule
