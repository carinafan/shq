# this is the standard SAM cleaning script but with "episodic" items renamed as "episodic"
# and shqID instead of subjectID

# given a dataframe with raw SAM scores, this script will calculate the total domain scores for episodic, semantic, spatial, and future
# the input dataframe should be called df.sam
	# the first column should contain subject IDs
	# the remaining columns should be named sam_DOMAIN_ITEMNUMBER, with an r at the end if reverse coded
		# e.g., sam_episodic_1r or sam_spatial_2
	# the columns should be in alphanumeric order
		# i.e., the episodic items first, then future, then semantic, then spatial
# this script requires the dplyr package to be loaded


#================================================================================================


# weights for each item

## episodic

weights.e1 = c(4.034189, -0.72426, -3.26156, -3.26156, -3.26156) 
weights.e2 = c(4.595144, 0.148222, -1.89281, -2.65545, -2.65545)
weights.e3 = c(-3.28431, -3.28431, -1.69751, 0.468724, 4.417257)
weights.e4 = c(-3.61453,  -1.65929, 0.063666, 1.4151, 4.520985)
weights.e5 = c(-4.24917,  -4.24917, -1.63086, 0.484567, 4.830579)
weights.e6 = c(-4.23944, -4.23944, -1.64721, 0.610062, 5.149669)
weights.e7 = c(-2.639, -0.65744, 0.872654, 3.918135, 3.918135)
weights.e8 = c(-3.05718, -3.05718, 0.212106, 0.212106, 5.410391)

## future

weights.f1 = c(-3.65111, -3.65111, -3.65111, -1.03676, 4.978371)
weights.f2 = c(-3.37011, -3.37011, -3.37011, -1.15237, 5.210321)
weights.f3 = c(-2.92213, -2.92213, -2.92213, -1.62935, 5.428823)
weights.f4 = c(-3.44228, -3.44228, -3.44228, -2.56174, 3.138971)
weights.f5 = c(-4.77243, -4.77243, -4.77243, -2.40095, 4.330484)
weights.f6 = c(3.365001, -1.40911, -3.86677, -3.86677, -3.86677)

## semantic

weights.s1 = c(-3.67249, -3.67249, -3.67249, 0.31115, 4.952858)
weights.s2 = c(7.619282, 0.339699, -0.79503, -3.95093, -3.95093)
weights.s3 = c(-3.35188, -3.07668, -1.2223, 3.306677, 7.477642)
weights.s4 = c(-4.7907, -4.7907, -0.27964, -0.04189, 6.926096)
weights.s5 = c(6.001116, -0.82331, -3.48742, -3.48742, -3.48742)
weights.s6 = c(-5.17872, -5.17872, -1.97152, 0.053879, 5.819628)

## spatial

weights.p1 = c(-6.75853, -6.75853, -0.54318, 1.659153, 5.795545)
weights.p2 = c(-6.81517, -6.81517, -6.81517, -0.29026, 4.682019)
weights.p3 = c(2.181895, 2.181895, 0.053028, -2.87437, -2.87437)
weights.p4 = c(3.84382, -1.82963, -7.18458, -7.18458, -7.18458)
weights.p5 = c(-7.25521, -7.25521, -7.25521, -1.25362, 3.875867)
weights.p6 = c(0.619688, 0.619688, 0.619688, -0.49716, 0.25509)


## combine weights into dataframe
weights = 
	rbind(weights.e1, weights.e2, weights.e3, weights.e4, weights.e5, weights.e6, weights.e7, weights.e8,
		  weights.f1, weights.f2, weights.f3, weights.f4, weights.f5, weights.f6,
		  weights.s1, weights.s2, weights.s3, weights.s4, weights.s5, weights.s6,
		  weights.p1, weights.p2, weights.p3, weights.p4, weights.p5, weights.p6) %>%
	as.data.frame()

# calculate weighted scores for each item

## initialize empty dataframe for weighted scores
df.sam.weighted = matrix(nrow = nrow(df.sam), ncol = ncol(df.sam)) %>% as.data.frame()

### change class to numeric 
df.sam.weighted = lapply(df.sam.weighted, function(x) as.numeric(x)) %>% as.data.frame()

### add subject IDs to empty dataframe 
df.sam.weighted[ , 1] = df.sam[ , 1]

### column names
names(df.sam.weighted) = c("shqID",
	"episodic_1w", "episodic_2w", "episodic_3w", "episodic_4w", "episodic_5w", "episodic_6w", "episodic_7w", "episodic_8w",
	"future_1w", "future_2w", "future_3w", "future_4w", "future_5w", "future_6w",
	"semantic_1w", "semantic_2w", "semantic_3w", "semantic_4w", "semantic_5w", "semantic_6w",
	"spatial_1w", "spatial_2w", "spatial_3w", "spatial_4w", "spatial_5w", "spatial_6w")

## calculate weighted scores
for (row in 1:nrow(df.sam)) {
	for (col in 2:ncol(df.sam)) {
		switch(df.sam[row, col],
			"1" = {df.sam.weighted[row, col] = weights[col - 1, 1]},
			"2" = {df.sam.weighted[row, col] = weights[col - 1, 2]},
			"3" = {df.sam.weighted[row, col] = weights[col - 1, 3]},
			"4" = {df.sam.weighted[row, col] = weights[col - 1, 4]},
			"5" = {df.sam.weighted[row, col] = weights[col - 1, 5]}
			)
	}
}

# calculate total scores
df.sam$sam_epi = df.sam.weighted %>% select(starts_with("episodic"))  %>% rowSums() + 100
df.sam$sam_sem = df.sam.weighted %>% select(starts_with("semantic")) 	%>% rowSums() + 100
df.sam$sam_spa = df.sam.weighted %>% select(starts_with("spatial")) 	%>% rowSums() + 100
df.sam$sam_fut = df.sam.weighted %>% select(starts_with("future")) 		%>% rowSums() + 100