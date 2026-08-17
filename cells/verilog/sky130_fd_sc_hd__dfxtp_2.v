module sky130_fd_sc_hd__dfxtp_2 (
    input  CLK,
    input  D,
    output reg Q
);

    always @(posedge CLK) begin
        Q <= D;
    end

endmodule
