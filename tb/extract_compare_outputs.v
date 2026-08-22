`define STORED_OUTPUT_WIDTH 20

module store_valid_output(
    input clk,
    input [7:0] O,
    input rst_n,
    output [`STORED_OUTPUT_WIDTH*8-1:0] stored_O
);
    reg [`STORED_OUTPUT_WIDTH*8-1:0] stored;
    assign stored_O = stored;

    always @(posedge clk) begin
        if (!rst_n) begin
            stored <= 0;
        end else begin
            if (O != 8'b0) begin
                stored <= {stored[(`STORED_OUTPUT_WIDTH-1)*8-1:0], O};
            end
        end
    end
endmodule

module check_output(
    input [`STORED_OUTPUT_WIDTH*8-1:0] value,
    output matched
);
    localparam [`STORED_OUTPUT_WIDTH*8-1:0] OUT1 = "(* TWO STARS *)";
    localparam [`STORED_OUTPUT_WIDTH*8-1:0] OUT2 = "TRY AGAIN";
    localparam [`STORED_OUTPUT_WIDTH*8-1:0] OUT3 = "TWO NOT TOUCH";
    assign matched = !(value == OUT1 || value == OUT2 || value == OUT3);
endmodule

module checked_puzzle(
    input clk,
    input rst_n,
    input enable,
    input I,
    output matched,
    output success,
    output [`STORED_OUTPUT_WIDTH*8-1:0] storedOutput,
    output [7:0] actualOutput
);
    wire [7:0] O;
    wire [`STORED_OUTPUT_WIDTH*8-1:0] totalOutput;

    puzzle puz(
        .clk(clk),
        .rst_n(rst_n),
        .enable(enable),
        .I(I),
        .O(O),
        .success(success)
    );
    
    store_valid_output svo(
        .clk(clk),
        .O(O),
        .rst_n(rst_n),
        .stored_O(totalOutput)
    );

    check_output checkermod(
        .value(totalOutput),
        .matched(matched)
    );

    assign storedOutput = totalOutput;
    assign actualOutput = O;
endmodule