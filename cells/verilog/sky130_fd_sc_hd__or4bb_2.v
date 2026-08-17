module sky130_fd_sc_hd__or4bb_2 (
    input  A,
    input  B,
    input  C_N,
    input  D_N,
    output X
);

    assign X = (A | B | ~C_N | ~D_N);

endmodule
