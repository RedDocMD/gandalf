module sky130_fd_sc_hd__o21bai_2 (
    input  A1,
    input  A2,
    input  B1_N,
    output Y
);

    assign Y = ~((A1 | A2) & ~B1_N);

endmodule
