// module stimulus(clk, number[7:0]);
// module stimulus(clk, number);
module stimulus();
// module stimulus;

input clk;
output clk, load;
inout [7:0] number [4:0];
wire a1;
reg a2;
integer a3, a4;

`define shiftright2 3'b001

assign number[0] = 1'b1;
assign Word_Line_q1[1] = ~wrapout_s1[1] & phi1;
assign number[0] = {1'b1, 2'b01};
assign memtemp_v1 = 1 ? 2 : 3;
assign memtemp_v1 = ~Write_Mem_q1 ? memory_v1[decodenum_s1] : 24'bz;
assign b = $display("hey", 1);

initial
	begin
		$display(1, $display(2));
		$display(3);
		$dumpsvar;
		a = 2;

		#322 a = 2;
		#400;

	case (Mem_Pointer_s1)
		ident: discard;
		
		1'bz:	begin
			a = 2;
			b = 3;
		end

		default: $display("it's working!", 2);
	endcase

		if(before)
			a = 1;

		if(main)
			a = 2;

		else if(branch) begin
			a = 3;
			$dumpsvar;
		end

		else begin
			if(nested)
				b = 4;
			else
				b = 5;
		end

		for (i = 0; i < `NUM_KERNEL; i = i+1) var[i] = 1;
	end

	initial a = 2;

	mdl m0();
	mdl m1(1);
	mdl m2(1, 2);

initial begin
	case(a)
	endcase

	
	for (i = 0; i < 8; i = ProgCntr+1)
		var[i] = 8'b0;
end

initial	case(b) endcase
initial	#200;
initial	if(hey) a = 2;

always begin end
always @(Phi1) begin end
always @(posedge Phi1 or posedge ali) begin end
		
endmodule

`timescale 1 ns / 1 ps

module A1; endmodule
module B2; endmodule
