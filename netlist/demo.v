module adder_demo (
    A,
    B,
    S,
    clk,
    en,
    rst_n
);
  input A;
  input B;
  output S;
  input clk;
  input en;
  input rst_n;

  wire net6;
  wire net7;
  wire net8;
  wire net9;
  wire net10;
  wire net11;
  wire net12;
  wire net13;
  wire net14;
  wire net15;
  wire net16;
  wire net17;
  wire net18;
  wire net19;
  wire net20;
  wire net21;
  wire net22;
  wire net23;
  wire net24;
  wire net25;
  wire net26;
  wire net27;
  wire net28;
  wire net29;
  wire net30;
  wire net31;
  wire net32;
  wire net33;
  wire net34;
  wire net35;
  wire net36;
  wire net37;
  wire net38;
  wire net39;
  wire net40;
  wire net41;
  wire net42;
  wire net43;
  wire net44;
  wire net45;
  wire net46;
  wire net47;
  wire net48;
  wire net49;
  wire net50;
  wire net51;
  wire net52;
  wire net53;
  wire net54;
  wire net55;
  wire net56;
  wire net57;
  wire net58;
  wire net59;
  wire net60;
  wire net61;
  wire net62;
  wire net63;
  wire net64;
  wire net65;
  wire net66;
  wire net67;
  wire net68;
  wire net69;
  wire net70;
  wire net71;
  wire net72;
  wire net73;
  wire net74;
  wire net75;
  wire net76;
  wire net77;
  wire net78;
  wire net79;
  wire net80;
  wire net81;
  wire net82;

  sky130_fd_sc_hd__a21bo_2 sky130_fd_sc_hd__a21bo_2_0 (
    .A1(net6),
    .A2(net7),
    .B1_N(net8),
    .X(net9)
  );

  sky130_fd_sc_hd__a21boi_2 sky130_fd_sc_hd__a21boi_2_1 (
    .A1(net10),
    .A2(net11),
    .B1_N(net12),
    .Y(net13)
  );

  sky130_fd_sc_hd__a21o_2 sky130_fd_sc_hd__a21o_2_2 (
    .A1(net14),
    .A2(net15),
    .B1(net16),
    .X(net17)
  );

  sky130_fd_sc_hd__a31o_2 sky130_fd_sc_hd__a31o_2_3 (
    .A1(net18),
    .A2(net19),
    .A3(net20),
    .B1(net21),
    .X(net22)
  );

  sky130_fd_sc_hd__a31o_2 sky130_fd_sc_hd__a31o_2_4 (
    .A1(net6),
    .A2(net7),
    .A3(net23),
    .B1(net24),
    .X(net15)
  );

  sky130_fd_sc_hd__a31o_2 sky130_fd_sc_hd__a31o_2_5 (
    .A1(net14),
    .A2(net15),
    .A3(net25),
    .B1(net22),
    .X(net11)
  );

  sky130_fd_sc_hd__a31o_2 sky130_fd_sc_hd__a31o_2_6 (
    .A1(net26),
    .A2(net27),
    .A3(net28),
    .B1(net29),
    .X(net6)
  );

  sky130_fd_sc_hd__a31o_2 sky130_fd_sc_hd__a31o_2_7 (
    .A1(net30),
    .A2(net31),
    .A3(net32),
    .B1(net33),
    .X(net24)
  );

  sky130_fd_sc_hd__and2_2 sky130_fd_sc_hd__and2_2_8 (
    .A(net34),
    .B(net35),
    .X(net29)
  );

  sky130_fd_sc_hd__and2_2 sky130_fd_sc_hd__and2_2_9 (
    .A(net36),
    .B(net37),
    .X(net38)
  );

  sky130_fd_sc_hd__and2_2 sky130_fd_sc_hd__and2_2_10 (
    .A(net18),
    .B(net19),
    .X(net16)
  );

  sky130_fd_sc_hd__and2_2 sky130_fd_sc_hd__and2_2_11 (
    .A(net39),
    .B(net40),
    .X(net21)
  );

  sky130_fd_sc_hd__and2_2 sky130_fd_sc_hd__and2_2_12 (
    .A(net41),
    .B(net42),
    .X(net43)
  );

  sky130_fd_sc_hd__and2_2 sky130_fd_sc_hd__and2_2_13 (
    .A(net44),
    .B(net8),
    .X(net7)
  );

  sky130_fd_sc_hd__and2_2 sky130_fd_sc_hd__and2_2_14 (
    .A(net45),
    .B(net46),
    .X(net33)
  );

  sky130_fd_sc_hd__and3_2 sky130_fd_sc_hd__and3_2_15 (
    .A(net47),
    .B(net48),
    .C(net49),
    .X(net50)
  );

  sky130_fd_sc_hd__and4bb_2 sky130_fd_sc_hd__and4bb_2_16 (
    .A_N(net51),
    .B_N(net52),
    .C(net53),
    .D(net54),
    .X(net55)
  );

  sky130_fd_sc_hd__and4bb_2 sky130_fd_sc_hd__and4bb_2_17 (
    .A_N(net56),
    .B_N(net43),
    .C(net50),
    .D(net55),
    .X(S)
  );

  sky130_fd_sc_hd__clkbuf_16 sky130_fd_sc_hd__clkbuf_16_18 (
    .A(net57),
    .X(net58)
  );

  sky130_fd_sc_hd__clkbuf_16 sky130_fd_sc_hd__clkbuf_16_19 (
    .A(clk),
    .X(net57)
  );

  sky130_fd_sc_hd__clkbuf_16 sky130_fd_sc_hd__clkbuf_16_20 (
    .A(net57),
    .X(net59)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_21 (
    .CLK(net58),
    .D(net60),
    .Q(net30),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_22 (
    .CLK(net58),
    .D(net61),
    .Q(net36),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_23 (
    .CLK(net59),
    .D(net62),
    .Q(net46),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_24 (
    .CLK(net59),
    .D(net63),
    .Q(net19),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_25 (
    .CLK(net58),
    .D(net64),
    .Q(net18),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_26 (
    .CLK(net58),
    .D(net65),
    .Q(net26),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_27 (
    .CLK(net58),
    .Q(net34),
    .RESET_B(rst_n),
    .D(net26)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_28 (
    .CLK(net58),
    .D(net66),
    .Q(net39),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_29 (
    .CLK(net59),
    .D(net67),
    .Q(net37),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_30 (
    .CLK(net59),
    .D(net68),
    .Q(net35),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_31 (
    .CLK(net59),
    .D(net69),
    .Q(net27),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_32 (
    .CLK(net59),
    .D(net70),
    .Q(net31),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_33 (
    .CLK(net58),
    .D(net71),
    .Q(net72),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_34 (
    .CLK(net58),
    .D(net73),
    .Q(net45),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_35 (
    .CLK(net59),
    .D(net74),
    .Q(net40),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__dfrtp_2 sky130_fd_sc_hd__dfrtp_2_36 (
    .CLK(net59),
    .D(net75),
    .Q(net76),
    .RESET_B(rst_n)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_37 (
    .A0(net36),
    .A1(net72),
    .S(en),
    .X(net61)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_38 (
    .A0(net46),
    .A1(net31),
    .S(en),
    .X(net62)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_39 (
    .A0(net19),
    .A1(net46),
    .S(en),
    .X(net63)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_40 (
    .A0(net30),
    .A1(net34),
    .S(en),
    .X(net60)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_41 (
    .A0(net76),
    .A1(net40),
    .S(en),
    .X(net75)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_42 (
    .A0(net37),
    .A1(net76),
    .S(en),
    .X(net67)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_43 (
    .A0(net26),
    .A1(B),
    .S(en),
    .X(net65)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_44 (
    .A0(net34),
    .A1(net26),
    .S(en)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_45 (
    .A0(net18),
    .A1(net45),
    .S(en),
    .X(net64)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_46 (
    .A0(net35),
    .A1(net27),
    .S(en),
    .X(net68)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_47 (
    .A0(net27),
    .A1(A),
    .S(en),
    .X(net69)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_48 (
    .A0(net31),
    .A1(net35),
    .S(en),
    .X(net70)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_49 (
    .A0(net45),
    .A1(net30),
    .S(en),
    .X(net73)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_50 (
    .A0(net39),
    .A1(net18),
    .S(en),
    .X(net66)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_51 (
    .A0(net72),
    .A1(net39),
    .S(en),
    .X(net71)
  );

  sky130_fd_sc_hd__mux2_1 sky130_fd_sc_hd__mux2_1_52 (
    .A0(net40),
    .A1(net19),
    .S(en),
    .X(net74)
  );

  sky130_fd_sc_hd__nand2_2 sky130_fd_sc_hd__nand2_2_53 (
    .A(net12),
    .B(net10),
    .Y(net77)
  );

  sky130_fd_sc_hd__nand2_2 sky130_fd_sc_hd__nand2_2_54 (
    .A(net72),
    .B(net76),
    .Y(net12)
  );

  sky130_fd_sc_hd__nand2_2 sky130_fd_sc_hd__nand2_2_55 (
    .A(net30),
    .B(net31),
    .Y(net8)
  );

  sky130_fd_sc_hd__nand2_2 sky130_fd_sc_hd__nand2_2_56 (
    .A(net26),
    .B(net27),
    .Y(net41)
  );

  sky130_fd_sc_hd__nor2_2 sky130_fd_sc_hd__nor2_2_57 (
    .A(net18),
    .B(net19),
    .Y(net78)
  );

  sky130_fd_sc_hd__nor2_2 sky130_fd_sc_hd__nor2_2_58 (
    .A(net36),
    .B(net37),
    .Y(net79)
  );

  sky130_fd_sc_hd__nor2_2 sky130_fd_sc_hd__nor2_2_59 (
    .A(net80),
    .B(net33),
    .Y(net23)
  );

  sky130_fd_sc_hd__nor2_2 sky130_fd_sc_hd__nor2_2_60 (
    .A(net81),
    .B(net21),
    .Y(net25)
  );

  sky130_fd_sc_hd__nor2_2 sky130_fd_sc_hd__nor2_2_61 (
    .A(net16),
    .B(net78),
    .Y(net14)
  );

  sky130_fd_sc_hd__nor2_2 sky130_fd_sc_hd__nor2_2_62 (
    .A(net39),
    .B(net40),
    .Y(net81)
  );

  sky130_fd_sc_hd__nor2_2 sky130_fd_sc_hd__nor2_2_63 (
    .A(net45),
    .B(net46),
    .Y(net80)
  );

  sky130_fd_sc_hd__nor2_2 sky130_fd_sc_hd__nor2_2_64 (
    .A(net38),
    .B(net79),
    .Y(net82)
  );

  sky130_fd_sc_hd__o21bai_2 sky130_fd_sc_hd__o21bai_2_65 (
    .A1(net79),
    .A2(net13),
    .B1_N(net38),
    .Y(net49)
  );

  sky130_fd_sc_hd__or2_2 sky130_fd_sc_hd__or2_2_66 (
    .A(net45),
    .B(net46),
    .X(net32)
  );

  sky130_fd_sc_hd__or2_2 sky130_fd_sc_hd__or2_2_67 (
    .A(net39),
    .B(net40),
    .X(net20)
  );

  sky130_fd_sc_hd__or2_2 sky130_fd_sc_hd__or2_2_68 (
    .A(net26),
    .B(net27),
    .X(net42)
  );

  sky130_fd_sc_hd__or2_2 sky130_fd_sc_hd__or2_2_69 (
    .A(net30),
    .B(net31),
    .X(net44)
  );

  sky130_fd_sc_hd__or2_2 sky130_fd_sc_hd__or2_2_70 (
    .A(net72),
    .B(net76),
    .X(net10)
  );

  sky130_fd_sc_hd__xnor2_2 sky130_fd_sc_hd__xnor2_2_71 (
    .A(net77),
    .B(net11),
    .Y(net47)
  );

  sky130_fd_sc_hd__xnor2_2 sky130_fd_sc_hd__xnor2_2_72 (
    .A(net41),
    .B(net28),
    .Y(net56)
  );

  sky130_fd_sc_hd__xnor2_2 sky130_fd_sc_hd__xnor2_2_73 (
    .A(net82),
    .B(net13),
    .Y(net48)
  );

  sky130_fd_sc_hd__xor2_2 sky130_fd_sc_hd__xor2_2_74 (
    .A(net14),
    .B(net15),
    .X(net53)
  );

  sky130_fd_sc_hd__xor2_2 sky130_fd_sc_hd__xor2_2_75 (
    .A(net34),
    .B(net35),
    .X(net28)
  );

  sky130_fd_sc_hd__xor2_2 sky130_fd_sc_hd__xor2_2_76 (
    .A(net25),
    .B(net17),
    .X(net54)
  );

  sky130_fd_sc_hd__xor2_2 sky130_fd_sc_hd__xor2_2_77 (
    .A(net6),
    .B(net7),
    .X(net52)
  );

  sky130_fd_sc_hd__xor2_2 sky130_fd_sc_hd__xor2_2_78 (
    .A(net23),
    .B(net9),
    .X(net51)
  );

endmodule
