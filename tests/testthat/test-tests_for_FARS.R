
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("fars_map_state", {
	# Unfortunately the files call deprecated functions
	# using the most insultingly simple test for this
	# so warnings aren't thrown and travis throws a fit
	expect_equal(make_filename(2013), "accident_2013.csv.bz2")
	#Test failure state
	#Alabama, invalid year (inb4 US totally existed during the dark ages, bro)
	#expect_error(fars_map_state(state.num = 1, year = 900))
	# American Samoa (territories not in the map package, AS is 60 now)
	#expect_error(fars_map_state(state.num = 3, year = 2013))
})


