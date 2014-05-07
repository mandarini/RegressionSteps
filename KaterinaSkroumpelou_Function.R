setwd("/Users/katerinaskroumpelou/Documents/UCL/coursework")
install.packages("rms")
library(rms)
library(ggplot2)
library(maptools)


modandov<-function(dataframe) {
	dataf<-dataframe
	print(colnames(dataf))
	#starts asking for variables, performs a check
	#that the input corresponds to a column name
	dependent<-readline("Which variable do you want to create an explanatory model for?")
	dependent<-as.character(dependent)
	cn0<-which(colnames(dataf)==dependent)
	while (length(cn0)==0) {
		dependent<-readline("Wrong input. Please choose a valid column.")
		dependent<-as.character(dependent)
		cn0<-which(colnames(dataf)==dependent)
	}
	v0<-dataf[,cn0]
	
	indep1<-readline("Choose first explanatory variable")
	indep1<-as.character(indep1)	
	cn1<-which(colnames(dataf)==indep1)
	while (length(cn1)==0) {
		indep1<-readline("Wrong input. Please choose a valid column.")
		indep1<-as.character(indep1)
		cn1<-which(colnames(dataf)==indep1)
	}
	v1<-dataf[,cn1]
	
	indep2<-readline("Choose second explanatory variable")
	indep2<-as.character(indep2)
	cn2<-which(colnames(dataf)==indep2)
	while (length(cn2)==0) {
		indep2<-readline("Wrong input. Please choose a valid column.")
		indep2<-as.character(indep2)
		cn2<-which(colnames(dataf)==indep2)
	}
	v2<-dataf[,cn2]
	
	indep3<-readline("Choose third explanatory variable")
	indep3<-as.character(indep3)
	cn3<-which(colnames(dataf)==indep3)
	while (length(cn3)==0) {
		indep3<-readline("Wrong input. Please choose a valid column.")
		indep3<-as.character(indep3)
		cn3<-which(colnames(dataf)==indep3)
	}
	v3<-dataf[,cn3]
	
	indep4<-readline("Choose fourth explanatory variable")
	indep4<-as.character(indep4)
	cn4<-which(colnames(dataf)==indep4)
	while (length(cn4)==0) {
		indep4<-readline("Wrong input. Please choose a valid column.")
		indep4<-as.character(indep4)
		cn4<-which(colnames(dataf)==indep4)
	}
	v4<-dataf[,cn4]
	
	indep5<-readline("Choose fifth explanatory variable")
	indep5<-as.character(indep5)
	cn5<-which(colnames(dataf)==indep5)
	while (length(cn5)==0) {
		indep5<-readline("Wrong input. Please choose a valid column.")
		indep5<-as.character(indep5)
		cn5<-which(colnames(dataf)==indep5)
	}
	v5<-dataf[,cn5]
	
	indep6<-readline("Choose sixth explanatory variable")
	indep6<-as.character(indep6)
	cn6<-which(colnames(dataf)==indep6)
	while (length(cn6)==0) {
		indep6<-readline("Wrong input. Please choose a valid column.")
		indep6<-as.character(indep6)
		cn6<-which(colnames(dataf)==indep6)
	}
	v6<-dataf[,cn6]
	
	wholeframe<-data.frame(v1, v2, v3, v4, v5, v6)
	model <-ols(data=dataf, v0 ~ v1+v2+v3+v4+v5+v6)
	modelbe<-fastbw(model, rule="p", type="residual", sls=.005, aics=0, eps=1e-9, k.aic=2, force=NULL)

#up to here i have found the best model out of the 6 variables that were input
#i chose to have the user input up to 6 variables, but that can change

#now, what i want to do is create a new data frame, called new data, that will have only the chosen variables
	
	len<-length(modelbe$names.kept)
#here i just want the size of the columns, so it is independent of which column i say
	siz<-length(dataf[,cn0])
	whole<-length(dataf)
	
	#i want my new data set to have double the columns of the chosen variables, so i can calculate the new ones multiplied with the coefficents, plus one extra, to contain the value of my predicted variable after i also add the intercept
	numcol<-(len*2)+1
	#initializing the data frame
	newdata <- data.frame(matrix(ncol = numcol, nrow = siz))

#here i fill the dataframe
	for (i in 1:len) {
		ind<-which(colnames(wholeframe)==modelbe$names.kept[i])
		for (j in 	1:siz) {
			newdata[,i]<-data.frame(wholeframe[,ind])
		}
	}

#here i calculate my new columns
	for (i in 1:len) {
		for (j in 1:siz) {
			newdata[j,len+i]<-newdata[j,i]*(as.numeric(modelbe$coefficients[i+1]))
		}
	}
	
	sizenew<-length(newdata)
	new<-len+1
	sizen<-sizenew-1
	intercept<-as.numeric(modelbe$coefficients[1])

#here i calculate the predicted variable by adding the intercept
	for (j in 1:siz) {
		newdata[j,sizenew]<-sum(newdata[j,new:sizen])+intercept
	}
	

#here i want to create two overlayed histograms
#so, i create two new dataframes with the old value and the new value
	newone<-data.frame(dependent=newdata[,sizenew])
	oldone<-data.frame(dependent=dataf[,cn0])

#here i apply an identity, so that when i bind them i know which is which
	newone$ident<-'predicted_value'
	oldone$ident<-'actual_value'


	#here i bind my two datasets so that i have one, to plot overlayed
	depen<-rbind(newone, oldone)
	#here i plot my overlayed histograms
	final<-ggplot(depen, aes(dependent, colour="black", fill = ident)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')+ggtitle("Overlayed Histograms of Actual and Modeled Variable")
	plot(final)
	ggsave("OverlayedHistograms.png", dpi=300)
	
	#now i want to make the output
	#I want the output to connect the kept variables with the coefficient values
	#and their indices in the initial dataframe
	
	temp<-data.frame(indices=c(cn1, cn2, cn3, cn4, cn5, cn6))
	temp2<-data.frame(names=c("v1", "v2", "v3", "v4", "v5", "v6"))
	temp$names<-temp2

	new<-data.frame(modelbe$coefficients)
	lenew<-length(new$modelbe.coefficients)

	new$indices[1]<-cn0
	for (i in 2:lenew) {
		d<-which(temp$names==rownames(new)[i])
		new$indices[i]<-temp[d,1]
	}
	
	
	print("Of the variables you chose, these are the ones kept in the final model:")
	for (i in 2:lenew){
		ind<-as.integer(new$indices[i])
		print(colnames(dataf)[ind])
		rownames(new)[i]<-colnames(dataf)[ind]
	}
	return(new)
}



newvalue<-function(dataframe, result_of_modandov, variable_changing, factor_of_change) {
	dataf<-dataframe
	model<-result_of_modandov
	#here, again, i isolate the variables that are in the model
	#i make a new dataframe containing the variables
	len<-length(model[,1])
	#rownames(model)[2:len]
	siz<-length(dataf[,1])
	whole<-length(dataf)
	newframe<- data.frame(matrix(ncol = len, nrow = siz))
	for (i in 2:len) {
		for (j in 	1:siz) {
			ind<-as.integer(model$indices[i])
			newframe[j,i-1]<-data.frame(dataf[j,ind])
		}
	}
	#renaming the columns, so that it is easier to correspond
	colnames(newframe)<-rownames(model)[2:len]

	#now, i calculate the predicted value, but altered accordingly
	change<-as.character(variable_changing)
	#i find the index of the column name, and i will take it from my other data frame
	#this is the index of the indices column, that is the index of my initial dataframe
	index2<-which(colnames(dataf)==change)
	#this is the index of index2 in the model dataframe, that corresponds to the column of the variable
	index3<-which(model$indices==index2)
	index4<-which((rownames(model)[index3])==colnames(newframe))
	
	factor<-factor_of_change
	#first we alter the variable in the newframe
	for (j in 1:siz) {
		newframe[j,index4]<-newframe[j,index4]*factor
	}			
	
	for (i in 1:len) {
		for (j in 1:siz) {
			newframe[j,i]<-newframe[j,i]*(as.numeric(model$modelbe.coefficients[i+1]))
		}
	}
	print(head(newframe))
	#now we calculate the final column which is the predicted variable
	forsum<-len-1
	interc<-as.numeric(model$modelbe.coefficients[1])
	for (j in 	1:siz) {
			newframe[j,len]<-sum(newframe[j,1:forsum])+interc
	}
	colnames(newframe)[len]<-"changed_prediction"
	return(newframe)
}

plots<-function(dataframe, result_of_newvalue, result_of_modandov, outline_dataframe) {
 	dataf<-dataframe
 	newv<-result_of_newvalue
 	conti<-result_of_modandov
 	ind<-conti[1,2]
  	siz<-length(mydata$x)
 	extraframe<-data.frame(matrix(ncol = 4, nrow = siz))
 	extraframe$x<-dataf$x
 	extraframe$y<-dataf$y
 	extraframe$prediction<-newv$changed_prediction
 	extraframe$current<-dataf[,ind]
 	#calculating the median value, this is what my midpoint is going to be
 	mid<-median(extraframe$prediction)
 	mid2<-mean(mydata[,ind])
 	outline<-outline_dataframe
 	plt<-ggplot(extraframe, aes(x=x, y=y))+geom_point(aes(colour=prediction)) + scale_colour_gradient2(low="maroon", mid="white", high="darkgreen", midpoint=mid, space ="rgb", na.value="grey80", guide="colourbar", guide_legend(title="Prediction if Changed"))+ggtitle("Map of Values when the variable is changed")
 	plot1<-plt+geom_path(data=outline, aes(long, lat, group=id), colour="black", size=0.05)+coord_equal()+labs(x="Eastings", y="Northings")
 	
 	plt2<-ggplot(extraframe, aes(x=x, y=y))+geom_point(aes(colour=current)) + scale_colour_gradient2(low="maroon", mid="white", high="darkgreen", midpoint=mid2, space="rgb", na.value="grey80", guide_legend(title="Current Value"))+ggtitle("Map of Current Values")
 	plot2<-plt2+geom_path(data=outline, aes(long, lat, group = id), colour="black", size=0.05)+coord_equal()+labs(x="Eastings", y="Northings")
 	
 	plot(plot2)
 	ggsave("MapOfCurrentValues.png", dpi=300)
 	plot(plot1)
 	ggsave("MapOfPredictedValues.png", dpi=300)
 }

mydata<-read.csv("LondonData.csv")
result1<-modandov(mydata)
result2<-newvalue(mydata, result1, "absence", 0.8)

#if i want to go on and plot the results, i must load a shapefile
#here i load my shapefile 
london<-readShapePoly("England_dt_2001_clipped.shp")
#and here i fortify it so i can use it in ggpplot2
londonoutline<-fortify(london, region = "NAME")

plots(mydata, result2, result1, londonoutline)



