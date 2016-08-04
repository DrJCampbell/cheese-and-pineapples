# cheese-and-pineapples
R function to create cheese-and-pineapple plots

Example usage:

	library("cheese-and-pineapples.r")
	
	x <- read.table(
		file="stats_output.txt",
		sep="\t",
		header=TRUE,
		stringsAsFactors=FALSE
		)
	
	# x should contain a column with p-value, a column with
	# effect sizes and a column with (gene) names
	
	make_cheese_pinapple_plot(
		x[which(
			x$pvalue_high < 0.05
			),],
		marker_name="RB1",
		pval_colname="pvalue_high",
		effect_colname="difference_high",
		names_colname="symbol",
		effect_direction="both",
		effect_scale_factor=1,
		output_file="cheese_pineapple_RB1.pdf",
		cex_adjust=1,
		centerx=12,
		centery=12,
		max_points=50,
		lab_extn_stretch=1.5,
		main="Cheese and Pineapple\nExample"
		)

