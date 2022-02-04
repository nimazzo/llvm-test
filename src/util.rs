pub fn optionize(context: (usize, usize)) -> (Option<usize>, Option<usize>) {
    (Some(context.0), Some(context.1))
}
