module sky130_fd_sc_hd__o2bb2a_2 (
    input  A1_N,
    input  A2_N,
    input  B1,
    input  B2,
    output X
);

    assign X = ((~A1_N | ~A2_N) & (B1 | B2));

endmodule
