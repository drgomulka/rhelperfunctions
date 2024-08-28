df_a <- data.frame(a=1:5)
df_ab <- data.frame(a=1:5, b=5:1)
dput(df_a)
dput(df_ab)
df_a[c(1, 2),]
df_ab[c(1, 2),]
dput( df_a[c(1, 2),] )
dput (df_ab[c(1, 2),] )

# look what happens:
> df_a <- data.frame(a=1:5)
> df_ab <- data.frame(a=1:5, b=5:1)
> dput(df_a)
structure(list(a = 1:5), class = "data.frame", row.names = c(NA, 
-5L))
> dput(df_ab)
structure(list(a = 1:5, b = 5:1), class = "data.frame", row.names = c(NA, 
-5L))
> df_a[c(1, 2),]
[1] 1 2
> df_ab[c(1, 2),]
  a b
1 1 5
2 2 4
> dput( df_a[c(1, 2),] )
1:2
> dput (df_ab[c(1, 2),] )
structure(list(a = 1:2, b = 5:4), row.names = 1:2, class = "data.frame")
> 
