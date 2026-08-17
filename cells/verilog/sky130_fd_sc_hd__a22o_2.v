module sky130_fd_sc_hd__a22o_2 (
    input  A1,
    input  A2,
    input  B1,
    input  B2,
    output X
);

    assign X = ((A1 & A2) | (B1 & B2));

endmodule
