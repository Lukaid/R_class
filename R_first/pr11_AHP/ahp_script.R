
install.packages('formattable')
install.packages("DiagrammeR")
install.packages ("D:\\ahp_0.2.12.tar.gz", repos=NULL, type="source") 



#install.packages("ahp")
library("ahp")
library("data.tree")

#job
list.files(system.file("extdata", package="ahp"))
ahpFile <- system.file("extdata", "job_choice.ahp", package = "ahp")
myAhp_job <- Load(ahpFile)
Calculate(myAhp_job)
print(myAhp_job, priority = function(x) x$parent$priority["Total", x$name])
Visualize(myAhp_job)
Analyze(myAhp_job)
AnalyzeTable(myAhp_job)



#car
list.files(system.file("extdata", package="ahp"))
ahpFile <- system.file("extdata", "car.ahp", package = "ahp")
myAhp_car <- Load(ahpFile)
Calculate(myAhp_car)
print(myAhp_car, priority = function(x) x$parent$priority["Total", x$name])
Visualize(myAhp_car)
Analyze(myAhp_car)
AnalyzeTable(myAhp_car)


#car
list.files(system.file("extdata", package="ahp"))
ahpFile <- system.file("extdata", "vacation.ahp", package = "ahp")
myAhp_vacation.ahp <- Load(ahpFile)
Calculate(myAhp_vacation.ahp)
print(myAhp_vacation.ahp, priority = function(x) x$parent$priority["Total", x$name])
Visualize(myAhp_vacation.ahp)
Analyze(myAhp_vacation.ahp)
AnalyzeTable(myAhp_vacation.ahp)



