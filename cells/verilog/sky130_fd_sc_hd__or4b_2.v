module sky130_fd_sc_hd__or4b_2 (
    input  A,
    input  B,
    input  C,
    input  D_N,
    output X
);

    assign X = (A | B | C | ~D_N);

endmodule
