module sky130_fd_sc_hd__and4_2 (
    input  A,
    input  B,
    input  C,
    input  D,
    output X
);

    assign X = (A & B & C & D);

endmodule
