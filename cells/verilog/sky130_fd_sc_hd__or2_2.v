module sky130_fd_sc_hd__or2_2 (
    input  A,
    input  B,
    output X
);

    assign X = (A | B);

endmodule
