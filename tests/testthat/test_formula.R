
# fs_sub ------------------------------------------------------------------

test_that('fs_sub works with atomic substitution', {
    fos <- list(a ~ b + c, a ~ e + c)
    new_fos <- fs_sub(fos, 'a', c(1, 2))

    expect_equivalent(new_fos[[1]], 1 ~ b + c)
    expect_equivalent(new_fos[[2]], 2 ~ b + c)
    expect_equivalent(new_fos[[3]], 1 ~ e + c)
    expect_equivalent(new_fos[[4]], 2 ~ e + c)
})

test_that('fs_sub works with list substitution', {
    fos <- list(a ~ b + c, a ~ e + c)
    new_fos <- fs_sub(fos, 'a', list('d + e', as.name('f + g')))

    expect_equivalent(new_fos[[1]], "d + e" ~ b + c)
    expect_equivalent(new_fos[[2]], `f + g` ~ b + c)
    expect_equivalent(new_fos[[3]], "d + e" ~ e + c)
    expect_equivalent(new_fos[[4]], `f + g` ~ e + c)
})



test_that('fs_sub works with just formula passed', {
    fos <- a ~ b + c
    new_fos <- fs_sub(fos, 'b', list('d + e', as.name('f + g')))

    expect_equivalent(new_fos[[1]], a ~ "d + e" + c)
    expect_equivalent(new_fos[[2]], a ~ `f + g` + c)

})
# f_subs ------------------------------------------------------------------

test_that('f_subs works uncrossed', {

    fo <- a ~ b + c
    subs <- list(a = c(1, 2), b = list('c+d', as.name('f + g')))

    new_fos = f_subs(fo, subs, cross = FALSE)

    expect_equivalent(new_fos[[1]], 1 ~ b + c)
    expect_equivalent(new_fos[[2]], 2 ~ b + c)
    expect_equivalent(new_fos[[3]], a ~ "c+d" + c)
    expect_equivalent(new_fos[[4]], a ~ `f + g` + c)
})


test_that('f_subs works crossed', {

    fo <- a ~ b + c
    subs <- list(a = c(1, 2), b = list('c+d', as.name('f + g')))

    new_fos = f_subs(fo, subs, cross = TRUE)

    expect_equivalent(new_fos[[1]], 1 ~ "c+d" + c)
    expect_equivalent(new_fos[[2]], 1 ~ `f + g` + c)
    expect_equivalent(new_fos[[3]], 2 ~ "c+d" + c)
    expect_equivalent(new_fos[[4]], 2 ~ `f + g` + c)
})
