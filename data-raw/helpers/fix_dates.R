fe_ind <- c(1414)
fn_ind <- c(483, 647, 962, 1187, 1330)

dataset[fe_ind, fecha_evaluacion := as.Date("2022-02-15")]
dataset[fn_ind, fecha_nacimiento := c("42483", "43530", "43483", "43599", "43862")]

rm(fe_ind, fn_ind)
