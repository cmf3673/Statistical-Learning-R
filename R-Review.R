# Brief R review #
##################

# sequences
y = seq(0, 10, 2)
y
# R does vector operations in parallel
x = c(1, 2, 4, 5, 8, 1)
z = y + x
z

# R does support negative subscripts and vector subscripts for subsets
z[-2] # returns {z} - {z[2]}
z[c(2, 4)] # returns {z[2], z[3]} so noninclusive
z[2:4] # returns {z[2], z[3], z[4]} so inclusive
# R doesn't use 0 based indexing!!

# martricies (m rows, n columns)
m = matrix(seq(1, 12), 4, 3) #(numbers, m, n)
m
# Goes by each column first

# subsetting matrix 
m[3:4, 1:3] #[rows, columns] inclusive
m[,3] # returns vector
m[,3, drop = F] # returns 4 x 1 matrix
ls() # gives vars in working directory 
rm(y) # removes y from working directory

# generate random data
udata = runif(30) # gets random data from uniform distribution
udata
ndata = rnorm(30) # gets random data from normal distribution
ndata
plot(udata, ndata, pch = 11, col = 'purple')
par(mfrow = c(2, 1)) # lets you set m x n for a group of plots in the same graphic (2 row plots in this case)
plot(udata, ndata)
hist(ndata) # adds to single graphic

# reading in data
df = read.csv('/Users/cf/Desktop/Fall 2021/NLP and ML/reddit_vm.csv')
df
names(df)
dim(df)
class(df) # gets object type
summary(df) # gives summary for each variable

# subsetting dataframe with bool statement
row = df[which(df$score == -12),]
row$body

# plotting
par(mfrow = c(1,1))
plot(df$created, df$score, ylim = c(-20, 100))
plot(df$created, df$comms_num)
attach(df) # this gives us cols as variables, so we don't have to keep pulling from df
created
search() # gives us workspaces,global is default
plot(created, score, ylim = c(-20, 100))



