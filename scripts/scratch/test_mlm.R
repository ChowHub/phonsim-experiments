N = 1000
subject = rep(rnorm(N/2), each=2)
pre_post = c(0, 2)
task = rep(rnorm(10, sd=.01), each=N/10)
error = rnorm(N, sd=.25)
type = rep(c(1, 5), each=N/2) 
dv = subject + task + type + error + pre_post


df = data.frame(subject = rep(1:(N/2), each=2), 
                task    = rep(letters[1:10], each=N/10),
                type    = rep(c('x','y'), each=N/2),
                test    = c('pre', 'post'),
                dv      = dv
)

fit1 = lmer(dv ~ type*test + (1 | task:subject) + (1 | task), data=df)
fit2 = lmer(dv ~ (1 | task:subject) + (1 | task), data=df)
summary(fit1)
summary(fit2)
table(df[c('type', 'task')])
