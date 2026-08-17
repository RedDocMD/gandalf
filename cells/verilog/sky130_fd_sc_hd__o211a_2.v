module sky130_fd_sc_hd__o211a_2 (
    input  A1,
    input  A2,
    input  B1,
    input  C1,
    output X
);

    assign X = ((A1 | A2) & B1 & C1);

endmodule
