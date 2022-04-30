module stimulus(clk, number[7:0]);
// module stimulus(clk, number);
// module stimulus;

// input clk;
// input clk, load;
// input [7:0] number [4:0];
// assign number[0] = 1'b1;
// assign Word_Line_q1[1] = ~wrapout_s1[1] & phi1;
// assign number[0] = {1'b1, 2'b01};
// `define shiftright2 3'b001
// assign memtemp_v1 = ~Write_Mem_q1 ? memory_v1[decodenum_s1] : 24'bz;

	// always @(clock or posedge reset)
	// begin
		// if(load)
		// 	count = data;

		// else if(reset)
		// 	count = 4'b0001;

		// else begin
		// 	if(mod)
		// 		count = {count[2:0], count[3]};
		// 	else
		// 		count = {count[0], count[3:1]};
		// end
	// end

	assign b = $display("hey", 1);

endmodule
