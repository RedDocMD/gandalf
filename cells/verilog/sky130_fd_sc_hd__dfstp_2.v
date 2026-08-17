module sky130_fd_sc_hd__dfstp_2 (
    input  CLK,
    input  D,
    input  SET_B,
    output reg Q
);

    always @(posedge CLK or negedge SET_B) begin
        if (!SET_B)
            Q <= 1'b1;
        else
            Q <= D;
    end

endmodule
