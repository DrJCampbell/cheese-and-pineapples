#
# generic cheese and pineapple function
#


require(gplots)

# function from http://stackoverflow.com/questions/9314658/colorbar-from-custom-colorramppalette
# to draw a scale bar
color.bar <- function(
	lut,
	min,
	max=-min,
	nticks=11,
	ticks=round(seq(min, max, len=nticks), 2),
	title=''){
		scale = (length(lut)-1)/(max-min)
		plot(
		c(0,10),
		c(min,max),
		type='n',
		bty='n',
		xaxt='n',
		xlab='',
		yaxt='n',
		ylab='',
		main=title,
		cex.main=2
		)
	axis(2, ticks, las=1, cex=2, cex.axis=2)
	for (i in 1:(length(lut)-1)){
		y = (i-1)/scale + min
		rect(0,y,10,y+1/scale, col=lut[i], border=NA)
	}
}


make_cheese_pinapple_plot <- function(
	data,
	output_file="test.pdf",
	cex_adjust=1,
	centerx=12,
	centery=12,
	pval_colname="pvalue_high",
	effect_colname="difference_high",
	effect_direction="negative",
	effect_scale_factor=1,
	names_colname="symbol",
	main="",
	marker_name="",
	max_points=NA,
	lab_extn_stretch=1
	){
	
	if(nrow(data) == 0){
		break
	}
	
	pval_col <- which(colnames(data) == pval_colname)
	
	# get the marker we are dealing with, check we have
	# only one and clean up the name for printing
#	marker <- levels(as.factor(data$marker))
#	if(length(marker) != 1){
#		stop("please supply only one marker in the results")
#	}
#	marker_name <- strsplit(marker, "_", fixed=TRUE)[[1]][1]
	
	# order the rows by target permutation test p-value
	data <- data[order(data[,pval_col], decreasing=FALSE),]
	neglogps <- -log10(data[,pval_col])
	neglogps[which(neglogps > 6)] <- 6
	data <- cbind(data, neglogps)
	
	if(!is.na(max_points)){
		data <- data[1:max_points,]
		neglogps <- neglogps[1:max_points]
	}
	
	
	# clean up the target names
	targets <- data[,names_colname]
	i <- NULL
	target_names <- NULL
	for(i in 1:length(targets)){
		target_names[i] <- strsplit(
			targets[i], "_", fixed=TRUE
			)[[1]][1]
	}
	
	# find where to place the points (as a circle)
	# we calculate one more point than there are targets and then throw
	# away the extra position to leave space for the scale markers
	target.count <- nrow(data)
	center.x <- centerx
	center.y <- centery
	t <- seq(0,2*pi,length=target.count+2)
	t <- t[-1] # we skip the first rad value to leave space for the scale bar
	
	number_of_labels <- length(t)
	half_labels <- number_of_labels / 2
	if((number_of_labels %% 2) == 1){
		half_labels <- (number_of_labels - 1) / 2
	}
	label_extensions <- abs(
		(t[1:half_labels] - 1.5) * lab_extn_stretch
		)
	
	label_extensions_for_plot <- c(
		label_extensions,
		(1.55 * lab_extn_stretch),
		label_extensions
		)
	if((number_of_labels %% 2) == 0){
		label_extensions_for_plot <- c(
			label_extensions,
			label_extensions
			)
	}
	
	coords <- matrix(NA, nrow=length(neglogps), ncol=2)
	coords_labs <- matrix(NA, nrow=length(neglogps), ncol=2)
	i <- NULL
	for(i in 1:target.count){ 
		coords[i,1] <- center.x + (sin(t[i])*(neglogps[i]))
		coords[i,2] <- center.y + (cos(t[i])*(neglogps[i]))
		coords_labs[i,1] <- center.x + (sin(t[i]) * (6.5 + label_extensions_for_plot[i]))
		coords_labs[i,2] <- center.y + (cos(t[i]) * (6.5 + label_extensions_for_plot[i]))
	}

	# open a PDF for output.
	# If the radius multiplier is != 1 then adjust the size
	pdf(file=output_file,7,7)
	par(oma=c(0,0,0,0), mar=c(0,0,0,0))
	plot(
		NULL,
		NULL,
		xlim=c(0,24),
		ylim=c(0,24),
		xlab="",
		ylab="",
		xaxt="n",
		yaxt="n",
		bty="n"
		)
	
	text(
		center.x,
		center.y+9.3,
		main,
		cex=1.5
		)
	
	# draw circles at positions 1:6
	symbols(
		x=rep(center.x, times=6),
		y=rep(center.y, times=6),
		circles=seq(1,6,by=1),
		lty=2,
		add=TRUE,
		inches=FALSE
		)
	
	# draw values at the 12 o'clock position
	rect(
		center.x-1,
		center.y,
		center.x+1,
		center.y+(6),
		col="white",
		border="white"
		)
	
	# draw lines from target points to lables
	# and write label
	i <- NULL

	for(i in 1:(nrow(coords))){
		text_pos=2
		text_x_shift <- 0.2
		if(i <= nrow(coords)/2){
			text_pos=4
			text_x_shift <- -0.2
		}
		lines(
			c(coords[i,1],coords_labs[i,1]),
			c(coords[i,2],coords_labs[i,2]),
			lwd=2,
			col="grey"
			)
		text(
			(coords_labs[i,1] + text_x_shift),
			coords_labs[i,2],
			target_names[i],
			cex=1*cex_adjust,
			pos=text_pos
			)
	}
	
	# blank out the positions where we will later put the
	# target red points. This lets the scale bar show throug
	i <- NULL
	for(i in (nrow(coords):1)){
		
		points(
			coords[i,1],
			coords[i,2],
			pch=19,
			col="white",
			cex=2*cex_adjust
			)		
	}
	
	
	# mark the scale on the circles
	i <- NULL
	for(i in 2:6){
		text(
			center.x,
			center.y+(i),
			paste(
				"1e-",
				i,
				sep=""
				)
			)
	}
	
	# draw a ball the the center
	points(
		centerx,
		centery,
		pch=19,
		cex=12,
		col="white"
		)
	points(
		centerx,
		centery,
		pch=19,
		cex=12,
		col=rgb(0,0,1,0.5)
		)

	# draw points for the targets
	i <- NULL
	for(i in (nrow(coords):1)){
		
		effect <- effect_scale_factor * data[i, effect_colname]
		ball_colour <- NULL
		if(effect_direction == "negative"){
			alpha <- (-effect)
			if(alpha < 0){
				alpha <- 0
			}
			if(alpha > 1){
				alpha <- 1
			}
			ball_colour <- rgb(1,0,0, alpha)
		}
		
		if(effect_direction == "positive"){
			alpha <- (effect)
			if(alpha < 0){
				alpha <- 0
			}
			if(alpha > 1){
				alpha <- 1
			}
			ball_colour <- rgb(0,0,1, alpha)
		}		


		if(effect_direction == "both"){
			if(effect < 0){
				alpha <- (-effect)
				if(alpha < 0){
					alpha <- 0
				}
				if(alpha > 1){
					alpha <- 1
				}
				ball_colour <- rgb(1,0,0, alpha)
			}else{
				alpha <- (effect)
				if(alpha < 0){
					alpha <- 0
				}
				if(alpha > 1){
					alpha <- 1
				}
				ball_colour <- rgb(0,0,1, alpha)
			}
		}			

		
		points(
			coords[i,1],
			coords[i,2],
			pch=19,
			col=ball_colour,
			cex=2*cex_adjust
			)
		points(
			coords[i,1],
			coords[i,2],
			col=rgb(0,0,0,0.25),# put a grey border around the spoke
			cex=2*cex_adjust
			)
	}

	# add the marker name last so not obscured by targets
	text(
		centerx,
		centery,
		marker_name,
		cex=1.4
		)

# 	effect_colname="difference_high",
#	effect_direction="negative",
#	effect_scale_factor=1,

	# add a colour scale bar
	par(mai=c(1,1,1,4))
	breaks <- NULL
	mycol <- NULL
	if(effect_direction == "negative"){
		breaks=seq(-1, 0, by=0.1) 
		mycol <- colorpanel(
			n=length(breaks)-1,
			low="red",
			high="white"
			)
		color.bar(
			mycol,
			-1*(1/effect_scale_factor),
			0,
			title=effect_colname,
			nticks=11
			)
	}
	if(effect_direction == "positive"){
		breaks=seq(0, 1, by=0.1) 
		mycol <- colorpanel(
			n=length(breaks)-1,
			low="white",
			high="blue"
			)
		color.bar(
			mycol,
			0,
			1*(1/effect_scale_factor),
			title=effect_colname,
			nticks=11
			)
	}
	if(effect_direction == "both"){
		breaks=seq(-1, 1, by=0.1) 
		mycol <- colorpanel(
			n=length(breaks)-1,
			low="red",
			mid="white",
			high="blue"
			)
		color.bar(
			mycol,
			-1*(1/effect_scale_factor),
			1*(1/effect_scale_factor),
			title=effect_colname,
			nticks=11
			)		
	}

	dev.off()

} # end make_cheese_pinapple_plot

