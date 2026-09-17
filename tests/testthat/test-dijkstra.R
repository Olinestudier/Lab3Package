test_that("dijkstra calculates correct shortest paths", {
  expect_equal(dijkstra(wiki_graph, 1), c(0, 7, 9, 20, 20, 11))
  expect_equal(dijkstra(wiki_graph, 3), c(9, 10, 0, 11, 11, 2))
})

test_that("dijkstra handles invalid inputs properly", {
  expect_error(dijkstra(wiki_graph, 99))
  expect_error(dijkstra("not_a_df", 1))
})
