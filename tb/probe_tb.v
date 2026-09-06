`timescale 1ns/100ps

module tb;
    reg clk = 0;
    reg I_reg = 0;
    reg rst_n_reg = 0;
    reg enable_reg = 0;

    wire tb_I = I_reg;
    wire tb_rst_n = rst_n_reg;
    wire tb_enable = enable_reg;
    wire tb_success;

    integer i;

    // Ouput is: (* TWO STARS *)
    // localparam WIDTH = 123;
    // localparam [WIDTH-1:0] I_bits = 123'b000000010101000010000000000001010101000000000000101000000100000100000010000010100001000000010000001000001001000101000000011;

    // Output is: TWO NOT TOUCH
    // localparam WIDTH = 140;
    // localparam [WIDTH-1:0] I_bits = 140'b00001000010000010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;

    // localparam WIDTH = 123;
    // localparam [WIDTH-1:0] I_bits = 123'b111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;

    localparam WIDTH = 123;
    reg [WIDTH-1:0] I_bits_mem [0:0];

    wire [WIDTH-1:0] I_bits;
    assign I_bits = I_bits_mem[0];

    puzzle puz(
        .clk(clk),
        .I(tb_I),
        .success(tb_success),
        .rst_n(tb_rst_n),
        .enable(tb_enable)
    );

    always #5 clk = ~clk;

    initial begin
        $dumpfile("tb/waves_probe.vcd");
        $dumpvars(0, tb);
        $readmemb("tb/I_bits.txt", I_bits_mem);
    end

    initial begin
        repeat (2) @(posedge clk);

        for (i = WIDTH - 1; i >= 0; i = i - 1) begin
            @(posedge clk);
            I_reg <= I_bits[i];
            if (i == WIDTH - 1) begin
                rst_n_reg <= 1'b1;
                enable_reg <= 1'b1;
            end
        end

        @(posedge clk);
        repeat (20) @(posedge clk);
        $finish;
    end
endmodule