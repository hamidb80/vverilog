// module counters(c8_Phi1_q1, c8_Phi2_q2, c9_Phi1_q1, c9_Phi2_q2, 
// 		c3_Phi1_q1, c3_Phi2_q2, p3_Phi1_q1, p3_Phi2_q2,
// 		count8_s1[7:0], count9_s1[8:0], 
// 		count3_s1[2:0], mempoint3_s1[2:0], 
// 		wrapout_s1[8:0], Reset_s1);

//  input c9_Phi1_q1, c9_Phi2_q2;
//  input c8_Phi1_q1, c8_Phi2_q2; 
//  input c3_Phi1_q1, c3_Phi2_q2;
//  input p3_Phi1_q1, p3_Phi2_q2;
//  input Reset_s1;
//  output [7:0] count8_s1;
//  output [8:0] count9_s1;
//  output [8:0] wrapout_s1;
//  output [2:0] count3_s1;
//  output [2:0] mempoint3_s1;

//  counter9 WordLineCounter (c9_Phi1_q1, c9_Phi2_q2, count9_s1[8:0],
// 			   Reset_s1);

//  counter8 PixMuxCounter (c8_Phi1_q1, c8_Phi2_q2, 
// 			  count8_s1[7:0], Reset_s1);

//  counter3 KernelMuxCounter (c3_Phi1_q1, c3_Phi2_q2, 
// 			    count3_s1[2:0], Reset_s1);

//  counter3 Memory_Pointer (p3_Phi1_q1, p3_Phi2_q2, 
// 			  mempoint3_s1[2:0], Reset_s1);

//  wrapshifter shifter (count3_s1[2:0], count9_s1[8:0],
// 		      wrapout_s1[8:0]);

// endmodule



module counter8(Phi1, Phi2, state_s1[7:0], Reset_s1);

 input Phi1, Phi2;
 input Reset_s1;
 output [7:0] state_s1;

 reg [7:0] state_s1;
 reg [7:0] state_s2;

initial begin
	if (Phi1) begin
		if (Reset_s1)
			state_s2 = 8'b1;		
		else begin
			state_s2[7:1] = state_s1[6:0];
			state_s2[0] = state_s1[7];
		end
	end
end

initial begin
 if(Phi2)
    state_s1 = state_s2;
end
endmodule


// module counter9(Phi1, Phi2, state_s1[8:0], Reset_s1);
//  input Phi1, Phi2;
//  input Reset_s1;
//  output [8:0] state_s1;

//  reg [8:0] state_s1;
//  reg [8:0] state_s2;

// initial begin
// if (Phi1)
//  begin
//   if (Reset_s1)
//      state_s2 = 9'b100000000;		
//   else 
//      begin
// 	state_s2[7:0] = state_s1[8:1];
//  	state_s2[8] = state_s1[0];
//      end
//  end
// end

// initial begin
//  if(Phi2)
//     state_s1 = state_s2;
// end

// endmodule

// module counter3(Phi1, Phi2, state_s1[2:0], Reset_s1);
//  input Phi1, Phi2;
//  input Reset_s1;
//  output [2:0] state_s1;

//  reg [2:0] state_s1;
//  reg [2:0] state_s2;

// initial begin
// if (Phi1)
//  begin
//   if (Reset_s1)
//      state_s2 = 3'b1;		
//   else 
//      begin
// 	state_s2[2:1] = state_s1[1:0];
//  	state_s2[0] = state_s1[2];
//      end
//  end
// end

// initial begin
//  if(Phi2)
//     state_s1 = state_s2;
// end

// endmodule

// module wrapshifter(shiftcontrol_s1[2:0], instate_s1[8:0], outstate_s1[8:0]);
//  input [2:0] shiftcontrol_s1;
//  input [8:0] instate_s1;
//  output [8:0] outstate_s1;

//  wire [2:0] shiftcontrol_s1;
//  wire [8:0] instate_s1; 
//  reg  [8:0] outstate_s1;

// `define shiftright2  3'b001
// `define shiftright1  3'b010
// `define	noshift      3'b100

// initial 
//   case (shiftcontrol_s1)

// 	`noshift   :	 outstate_s1 = instate_s1;
	
// 	`shiftright1  :	
// 			begin
// 			 outstate_s1[7:0] = instate_s1[8:1];
// 			 outstate_s1[8] = instate_s1[0];
// 			end

// 	`shiftright2  :
// 			begin
// 			 outstate_s1[6:0] = instate_s1[8:2];
// 			 outstate_s1[8:7] = instate_s1[1:0];
// 			end

// 	default    :	 outstate_s1 = instate_s1;
//   endcase
// endmodule