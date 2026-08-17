module sky130_fd_sc_hd__o21ba_2 (
    input  A1,
    input  A2,
    input  B1_N,
    output X
);

    assign X = ((A1 | A2) & ~B1_N);

endmodule
