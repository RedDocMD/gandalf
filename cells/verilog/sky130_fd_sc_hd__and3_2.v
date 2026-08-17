module sky130_fd_sc_hd__and3_2 (
    input  A,
    input  B,
    input  C,
    output X
);

    assign X = (A & B & C);

endmodule
