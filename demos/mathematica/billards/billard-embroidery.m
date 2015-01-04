(* ::Package:: *)

(* ::Title:: *)
(*Billard Embroidery*)


(* ::Section:: *)
(*The Billiard Ball Function*)


(* ::Subsubsection:: *)
(*polygon billard inside a rectangle*)


Billard[w_Integer, h_Integer, steps_:Infinity] := 

	Block[
	
		{x=0,y=0,s = steps,step= 1, dx, dy, pts},

		fn[{x_, y_}]:= Block[

			{ds = Min[s, step]},

			If[x >= w, dx = -1];
			If[x <= 0, dx = +1];
			If[y >= h, dy = -1];
			If[y <= 0, dy = +1];
			s-= ds;

			{x + dx * ds, y+ dy * ds}
			
		];

		If[	s == Infinity,
			(*repeat until we get back to the start *)
			NestWhileList[fn,{x,y}, (#2 != {0,0})&, 2], 
			(* repeat while we have no thread left *)
			NestWhileList[fn,{x,y}, (s > 0) &]
		]
 	]


(* ::Section:: *)
(*Running Stitch Embroidery*)


(* ::Subsubsection:: *)
(*pattern matching*)


PointsQ[pts : {{_, _} .. }] := True


(* ::Subsubsection:: *)
(*stitch plot options*)


Options[StitchPlot] = { 
	Opacity -> 0.8, 
	StitchStyle-> {Black},
	FabricStyle -> { White},
	BackStitchStyle-> {Thickness[0.01],Gray},
	PlotRange->{{0,0},{1,1}}
};


(* ::Subsubsection:: *)
(*stitch plot function*)


StitchPlot[pts_?PointsQ,opts:OptionsPattern[{StitchPlot, Graphics}]]:= 

  Block[

	{
		range = OptionValue[PlotRange],
		opacity = OptionValue[Opacity],
		stitchStyle=OptionValue[StitchStyle],
		fabricStyle = OptionValue[FabricStyle],
		backStitchStyle=OptionValue[BackStitchStyle],
		gopts = FilterRules[{opts}, Options[Graphics]]
	},

	Graphics[
		{
			Thick,
			{  Sequence @@ backStitchStyle,Line /@ Partition[Rest[pts],2] },
			{ Opacity[opacity],EdgeForm[Thick], Sequence @@ fabricStyle, Rectangle @@ Transpose[range] } ,
			{  Sequence @@ stitchStyle, Line /@ Partition[pts,2] }
		},
		Sequence @@\[NonBreakingSpace]gopts
	]

  ]


(* ::Section:: *)
(*Putting it All Together*)


BillardPlot[w_, h_, steps_:Infinity ,opts: OptionsPattern[StichPlot]] /;(NumberQ[steps] || steps == Infinity ) := 
  
	Block[
		{s= Min[steps,LCM[w,h]]},
		StitchPlot[Billard[w, h, s], PlotRange-> {{0,w},{0,h}},  opts]
	]  



BillardAnimate[w_, h_, opts : OptionsPattern[StitchPlot]] := 

	Block[
		{billard = Billard[w, h]},
		Animate[BillardPlot[w, h, i, opts], {i, 0, LCM[w, h]}]
	]  


(* ::Section:: *)
(*Let' s Play!*)


(* ::Subsection:: *)
(*Simple Plotting*)


a = 35;b = 12; BillardPlot[a,b]


(* ::Subsection:: *)
(*Interactive Plotting*)


Manipulate[

	Dynamic[
		BillardPlot[a,b,i]
	],

	{{a,11, "Width"},1,100,1},
	{{b, 7, "Height"},1,100,1},
	{{i, Infinity, "Progress"},1,LCM[a,b],1}

]


(* ::Subsection:: *)
(*Improving the Interface*)


BillardExplorer[] := Manipulate[

(******************** DYNAMIC *********************)

	x = Round[p[[1]]];
	y = Round[p[[2]]];
	imax = LCM[x, y];

(******************** DISPLAY *********************)

  Show[
	
	{
		BillardPlot[
			x, y, i * imax,
			FabricStyle -> {fcolor},
			StitchStyle -> {tcolor},
			BackStitchStyle -> {bcolor},
			Opacity -> alpha
		]
	},

	Axes -> axes,
	GridLines -> If[grid, Automatic, None],
	PlotRange -> {{0, 100}, {0, 100}},
	ImageSize->Full

	],

 (********************* CONTROLS **********************)

	{{p, {97, 65}}, Locator},
	{{i, 1/2, "Progress"}, Slider[#, {0, 1, 1/imax}, ImageSize -> 440] &},

	Control[{{alpha, 1.0, "Opacity"}, Slider[#, {0, 1, .1}, ImageSize -> 440] &}],

	Row[{
		Control[{{tcolor, Black, "Thread"}, ColorSetter[#, ImageSize -> 100] &}],
		Spacer[15],
		Control[{{bcolor, Gray, "Backside"}, ColorSetter[#, ImageSize -> 100] &}],
		Spacer[15],
		Control[ {{fcolor, White, "Fabric"}, ColorSetter[#, ImageSize -> 100] &}] 
	}],

	Row[{
		Spacer[15],
		Control[{{axes, True, "Axes"}, {True, False}}],
		Spacer[15],
		Control[{{grid, True, "Grid"}, {True, False}}]
	}],

 (********************* LAYOUT **********************)

	ContentSize->{500, 300},
	ControlPlacement ->  Bottom,
	FrameLabel->{None, None, TextCell["Billard Stitch Explorer", "Chapter"]}

]



BillardExplorer[]
