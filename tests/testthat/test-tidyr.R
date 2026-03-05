m <- mt_sim_brownian_motion(tracks = letters[2:3], t = 1:10)
mt <- mt_read(mt_example())[7:60, ]

test_that("reframe and group modify works", {
  skip_if_not_installed("dplyr")
  dat <- mt_as_move2(dplyr::tibble(x = 1:30, y = runif(30), t = sort(runif(30)), id = gl(3, 10)),
    coords = c("x", "y"), time_column = "t", track_id_column = "id"
  )
  uu <- function(x) {
    # attributes seems to be added by reframe
    #    g = st_sfc(st_point(1:2), st_point(3:4))
    #    s = st_sf(a=3:4, g)
    # s |> reframe(head(pick(everything())))  |> pull(g) |> attributes()
    # s |> pull(g) |> attributes()
    attr(x$geometry, "classes") <- NULL
    x
  }
  expect_equal(
    dat |> group_by(id) |> reframe(head(pick(everything()), 3)) |> tibble::remove_rownames() |>
      uu(),
    ## change order column as reframe does this by default (see ?do example)
    dat[1:3 + rep((0:2) * 10, each = 3), 2:1]
  )
  expect_equal(
    dat |> group_by(id) |> group_modify(head, n=3) |> ungroup(),
    ## change order column as reframe does this by default (see ?do example)
    dat[1:3 + rep((0:2) * 10, each = 3), 2:1]
  )
  expect_equal(dat |> dplyr::group_modify(head, n=13), head(dat, 13))
  expect_equal(dat |> dplyr::group_map(head, n=13), list(head(dat, 13)))

  expect_equal(
    dat |> group_by(t>.5) |> dplyr::group_map(~.x, .keep=FALSE),
    list(dat[!(dat$t>.5),],dat[(dat$t>.5),])
  )

  expect_equal(
    dat |> group_by(id) |> dplyr::group_map(~.x,.keep=T),
    dat |> group_by(id) |> dplyr::group_split())

  expect_equal(
    dat |> group_by(id) |> dplyr::group_map(~.x),
    dat |> group_by(id) |> dplyr::group_split())
  expect_equal(
    dat |> group_by(id) |> dplyr::group_map(head, n=3) |> mt_stack() ,
    dat[1:3 + rep((0:2) * 10, each = 3),]
  )


  fish <- mt_read(mt_example(), n_max = 200)
  expect_equal(
    fish |> group_by(as.Date(timestamp)) |>
      reframe(head(pick(everything()), 3)) |> sf::st_crs(),
    sf::st_crs(fish)
  )
})

test_that("testing tidy functions", {
  skip_if_not_installed("dplyr")
  expect_s3_class(m, "move2")
  expect_s3_class(m |> filter(time < 3), "move2")
  expect_identical(m |> mt_track_data() |> nrow(), 2L)
  expect_identical(m |> filter(time < 3) |> mt_track_data() |> nrow(), 2L)
  expect_named(m, c("time", "track", "geometry"), ignore.order = TRUE)
  expect_named(m |> mutate(coll = time), c("time", "track", "geometry", "coll"), ignore.order = TRUE)
  expect_identical(m |> mutate(coll = time * 2) |> filter(track == "c") |> pull(coll), 2 * (1:10))
})
test_that("slice", {
  skip_if_not_installed("dplyr")
  expect_identical(m |> slice(1:4) |> mt_track_data() |> nrow(), 1L)
  expect_s3_class(m |> slice(1:4), "move2")
  expect_s3_class(m |> slice(1:4) |> mt_track_data(), "data.frame")
})
test_that("rowwise", {
  skip_if_not_installed("dplyr")
  expect_identical(m |> mutate(mt = mean(time)) |> pull(mt), rep(mean(m$time), nrow(m)))
  expect_identical(
    m |> dplyr::rowwise() |> mutate(mt = mean(time)) |> pull(mt),
    as.double(m$time)
  )
  expect_identical(
    m |> dplyr::rowwise() |> mutate(mt = sum(time, time)) |> pull(mt),
    m$time * 2L
  )
  expect_s3_class(m |> dplyr::rowwise() |> mutate(mt = sum(time, time)), class = "move2")
})
test_that("grouped_df class is retained on row slice", {
  skip_if_not_installed("dplyr")
  expect_true(m %>% group_by(track) %>% slice(1) %>% inherits("grouped_df"))
  expect_true(m %>% group_by(track) %>% filter(time < 3) %>% inherits("grouped_df"))
  expect_true(mt %>% group_by(visible) %>% slice(1) %>% inherits("grouped_df"))
  expect_true(mt %>% group_by(visible) %>% filter(timestamp < 3) %>% inherits("grouped_df"))
})
test_that("select retains columns", {
  skip_if_not_installed("dplyr")
  expect_true("track" %in% (m |> select(-track) |> names()))
  expect_true("time" %in% (m |> select(-time) |> names()))
  expect_true("time" %in% (m |> select(-track) |> names()))
  expect_true("track" %in% (m |> select(-time) |> names()))
  expect_true("track" %in% (m |> select(track) |> names()))
  expect_true("time" %in% (m |> select(time) |> names()))
  expect_true("time" %in% (m |> select(track) |> names()))
  expect_true("track" %in% (m |> select(time) |> names()))
  expect_true("track" %in% (m |> select(geometry) |> names()))
  expect_true("time" %in% (m |> select(geometry) |> names()))
})

test_that("select work on aditional columns", {
  skip_if_not_installed("dplyr")
  expect_named(m |> mutate(extra = time),
    c("time", "track", "extra", "geometry"),
    ignore.order = TRUE
  )
  expect_named(
    m |>
      mutate(extra = time) |>
      select(extra), c("extra", "geometry", "time", "track"),
    ignore.order = TRUE
  )
  expect_named(
    m |> mutate(extra = time) |>
      select(-extra), c("time", "track", "geometry"),
    ignore.order = TRUE
  )
  expect_named(
    m |>
      mutate(extra = time) |>
      select(track), c("track", "geometry", "time"),
    ignore.order = TRUE
  )
})
test_that("id column retained in track data", {
  expect_named(
    m |> select_track_data(-track) |> mt_track_data(), "track"
  )
})

test_that("selection of column without column name", {
  expect_identical(m %>% mutate(aa = time), m %>% mutate(aa = mt_time()))
  expect_identical(m %>% mutate(aa = track), m %>% mutate(aa = mt_track_id()))
  expect_identical(m %>% group_by(aa = track), m %>% group_by(aa = mt_track_id()))
  expect_identical(m %>% group_by(aa = time), m %>% group_by(aa = mt_time()))
  expect_error(
    mt_time(),
    "`mt_time..` can only be used without a `x` argument inside dplyr verbs"
  )
  expect_error(
    mt_track_id(),
    "`mt_track_id..` can only be used without a `x` argument inside dplyr verbs"
  )
})
test_that("dplyr methods trackid and time", {
  expect_identical(
    m %>%
      dplyr::mutate(aa = mt_track_id()),
    dplyr::mutate(m, aa = track)
  )
  expect_identical(
    m %>%
      dplyr::mutate(aa = mt_time()) %>%
      pull(aa),
    mt_time(m)
  )
  expect_identical(
    m %>%
      dplyr::mutate(aa = mt_track_id()) %>%
      pull(aa),
    mt_track_id(m)
  )
})

test_that("group_split", {
  skip_if_not_installed("dplyr")
  m <- mt_sim_brownian_motion()
  expect_equal(
    m %>%
      dplyr::group_by(mt_track_id()) %>%
      dplyr::group_split(.keep = FALSE),
    split(m, mt_track_id(m)),
    ignore_attr = TRUE
  )
})

test_that("grouping corresponds", {
  m <- mt_sim_brownian_motion() |>
    mutate(tt = time < 5) |>
    dplyr::tibble() |>
    mt_as_move2(time_column = "time", track_id_column = "track") %>%
    mutate_track_data(sex = letters[1:2])
  expect_identical(
    m |> group_by_track_data(sex),
    (m |> group_by(sex = sort(rep(letters[1:2], 10))))
  )
  expect_identical(
    m |> group_by(tt) |> group_by_track_data(sex, .add = TRUE),
    (m |> group_by(tt, sex = sort(rep(letters[1:2], 10))))
  )
  expect_identical(
    m |> group_by(tt) |> group_by_track_data(sex, .add = FALSE),
    (m |> group_by(sex = sort(rep(letters[1:2], 10))))
  )
})
test_that("bind_cols and st_join return correct", {
  m <- mt_read(mt_example())[1000:1010, ]
  class(m) <- setdiff(class(m), "spec_tbl_df") # remove "spec_tbl_df" class
  dfa <- data.frame(x = sf::st_coordinates(m)[, 1], y = sf::st_coordinates(m)[, 2], new_colA = (letters[seq_len(nrow(m))]))
  df <- dfa[-c(1, 4, 7), ]
  df <- df[sample(seq_len(nrow(df))), ]
  dfsf <- sf::st_as_sf(df, coords = c("x", "y"), crs = st_crs(m))
  mdf <- mt_as_move2(data.frame(m), time_column = "timestamp", track_id_column = "individual.local.identifier")

  expect_s3_class(dplyr::bind_cols(mdf, dfa), class(mdf), exact = TRUE)
  expect_s3_class(dplyr::bind_cols(m, dfa), class(m), exact = TRUE)
  # expect_identical(class(cbind(mdf,dfa)),class(mdf))
  # expect_identical(class(cbind(m,dfa)),class(m))

  expect_s3_class((sf::st_join(mdf, dfsf)), class(mdf), exact = TRUE)
  expect_s3_class((sf::st_join(m, dfsf)), class(m), exact = TRUE)
  e <- letters[1L:11L]
  e[c(1, 4, 7)] <- NA
  expect_identical(sf::st_join(mdf, dfsf)$new_colA, e)
  expect_identical(sf::st_join(m, dfsf)$new_colA, e)
})


test_that("track_id order does not matter for `group_by_track_id",{
    dat<-mt_as_move2(data.frame(x=1:10, y=1, id=gl(5,2), t=1:10, extra=letters[1:10]),coords=1:2,time_column = 't', track_id_column = 'id' ) |> mutate_track_data(r=c(3,2,3,3,1), h=letters[1:5])
    expect_identical(dat |> group_by_track_data(r) |> dplyr::group_indices(), v<-c(3L,3L,2L,2L,3L,3L,3L,3L,1L,1L) )
    expect_named(dat |> group_by_track_data(r), c("id","t","extra","geometry","r"))
    expect_named(dat |> group_by_track_data(r,h), c("id","t","extra","geometry","r","h"))
    expect_named(dat |> group_by_track_data(h), c("id","t","extra","geometry","h"))

    dat_mix<-    mt_set_track_data(dat,mt_track_data(dat)[5:1,])
    expect_identical(dat_mix |> group_by_track_data(r) |> dplyr::group_indices(), v )
    expect_identical(dat |> group_by_track_data(r<2.5) |> dplyr::group_indices(), v<-c(1L,1L,2L,2L,1L,1L,1L,1L,2L,2L) )
    expect_identical(dat_mix |> group_by_track_data(r<2.5) |> dplyr::group_indices(), v<-c(1L,1L,2L,2L,1L,1L,1L,1L,2L,2L) )
    expect_named(dat |> group_by_track_data(r<2.5), c("id","t","extra","geometry","r < 2.5"))
    expect_identical(dat_mix |> group_by(t<3)|> group_by_track_data(r<2.5) |> dplyr::group_indices(), v<-c(1L,1L,2L,2L,1L,1L,1L,1L,2L,2L) )
    expect_identical(dat |> group_by(t<3)|> group_by_track_data(r<2.5) |> dplyr::group_indices(), v<-c(1L,1L,2L,2L,1L,1L,1L,1L,2L,2L) )
    expect_identical(dat |> group_by(t>3.4)|> group_by_track_data(r<2.5, .add = T) |> dplyr::group_indices(), v<-c(1L,1L,2L,4L,3L,3L,3L,3L,4L,4L) )

})
