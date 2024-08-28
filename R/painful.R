df_a <- data.frame(a=1:5)
df_ab <- data.frame(a=1:5, b=5:1)
dput(df_a)
dput(df_ab)
df_a[1,]
df_ab[1,]
dput( df_a[1,] )
dput (df_ab[1,] )

# look what happens:
> df_a <- data.frame(a=1:5)
> df_ab <- data.frame(a=1:5, b=5:1)
> dput(df_a)
structure(list(a = 1:5), class = "data.frame", row.names = c(NA, 
-5L))
> dput(df_ab)
structure(list(a = 1:5, b = 5:1), class = "data.frame", row.names = c(NA, 
-5L))
> df_a[1,]
[1] 1
> df_ab[1,]
  a b
1 1 5
> dput( df_a[1,] )
1L
> dput (df_ab[1,] )
structure(list(a = 1L, b = 5L), row.names = 1L, class = "data.frame")
