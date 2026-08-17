module sky130_fd_sc_hd__dfrtp_2 (
    input  CLK,
    input  D,
    input  RESET_B,
    output reg Q
);

    always @(posedge CLK or negedge RESET_B) begin
        if (!RESET_B)
            Q <= 1'b0;
        else
            Q <= D;
    end

endmodule
