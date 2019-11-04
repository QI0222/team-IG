##mediation analysis
pacman::p_load(mediation,quantreg)
data("jobs")
model.m<-lm(job_seek~treat+depress1+econ_hard+sex+age+occp+marital+nonwhite+educ+income,data=jobs)
model.y<-lm(depress2~treat+job_seek+depress1+econ_hard+sex+age+occp+marital+nonwhite+educ+income,data=jobs)
out.1<-mediate(model.m,model.y,sims=1000,boot=TRUE,treat = "treat",mediator="job_seek")
out.2<-mediate(model.m,model.y,sims=1000,treat = "treat",mediator="job_seek")
summary(out.2)
#In this case, we find that job search self-efficacy mediated the effect of the treatment on depression in the negative direction.
#This effect, however, was small with a point estimate of -0.014 and the 95% confidence interval barely covers 0.
plot(out.2)
model.y<-lm(depress2~treat+job_seek+treat:job_seek+depress1+econ_hard+sex+age+occp+marital+nonwhite+educ+income,data=jobs)
out.3<-mediate(model.m,model.y,sims=1000,treat="treat",mediator = "job_seek")
summary(out.3) #how to illustrate the result?
plot(out.3,treatment="both")
model.m<-lm(job_seek~treat+depress1+econ_hard+sex+age+occp+marital+nonwhite+educ+income,data=jobs)
model.y<-rq(depress2~treat+job_seek+depress1+econ_hard+sex+age+occp+marital+nonwhite+educ+income,data=jobs)
out.6<-mediate(model.m,model.y,sims=1000,boot=TRUE,treat="treat",mediator="job_seek")
summary(out.6)

#Discrete Mediator and Outcome Data
model.m<-lm(job_seek~treat+depress1+econ_hard+sex+age+occp+marital+nonwhite+educ+income,data=jobs)
model.y<-glm(work1~treat+job_seek+depress1+econ_hard+sex+age+occp+marital+nonwhite+educ+income,
             family=binomial(link="probit"),data=jobs)
out.7<-mediate(model.m,model.y,sims=1000,treat="treat",mediator="job_seek")
summary(out.7)

#Sensitivity analysis
sens.cont<-medsens(out.7,rho.by=0.05)
summary(sens.cont)
#R^2_M*R^2_Y is the product of coeeficients of determination which represents the proportion of the previously unexplained variance in the
#mediator and and outcome variabls that is explained by an unobservable pretreatment unconfounder.
plot(sens.cont,sens.par="rho")
#the dashed horizontal line represents the estimated mediation effect under the sequential ignorability assumption,
#and the solid line represents the mediation effect under various values of p. 
#The grey region represents the 95% confidence bands.
plot(sens.cont,sens.par = "R2",r.type="total",sign.prod = "negative")
