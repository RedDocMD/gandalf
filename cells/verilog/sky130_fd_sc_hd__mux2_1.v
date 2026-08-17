module sky130_fd_sc_hd__mux2_1 (
    input  A0,
    input  A1,
    input  S,
    output X
);

    assign X = S ? A1 : A0;

endmodule
