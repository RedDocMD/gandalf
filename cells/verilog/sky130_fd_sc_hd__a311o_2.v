module sky130_fd_sc_hd__a311o_2 (
    input  A1,
    input  A2,
    input  A3,
    input  B1,
    input  C1,
    output X
);

    assign X = ((A1 & A2 & A3) | B1 | C1);

endmodule
