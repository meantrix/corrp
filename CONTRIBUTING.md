
# Contributing to `corrp`

Thank you for your interest in contributing to the `corrp` package!

. We welcome contributions and improvements from the community. To help make the process smooth, please follow the guidelines outlined below.

## How to Contribute

### Reporting Bugs

If you encounter a bug or issue, please follow these steps:
1. **Search for existing issues** to see if the bug has already been reported.
2. If not, **open a new issue** on the [GitHub Issues page](https://github.com/meantrix/corrp/issues).
3. Be sure to include the following details:
   - A description of the issue.
   - Steps to reproduce the issue.
   - Any error messages or warnings.
   - The version of the package you're using.
   - The R version.
   - Operating system if relevant.


### Submitting Code

To submit code changes, please follow these steps:
1. **Fork the repository**.
2. **Create a new branch** for your changes.
3. **Implement your changes**. Be sure to:
   - Write clear and concise commit messages.
   - Document new code and functions using `roxygen2` comments.
   - Ensure your code follows the existing code style (e.g., indentation, naming conventions).

4. **Run tests** to ensure your changes work as expected. To do this, use the following command:

```R
rcmdcheck::rcmdcheck()
```

The result should show **one note** only, like this:

```
── R CMD check results ────────────────────────────────────────────────────────────────────────────────────────────────────────────────── corrp 0.5.0 ────
Duration: 57.5s

❯ checking installed package size ... NOTE
    installed size is 5.9Mb
    sub-directories of 1Mb or more:
      libs 5.6Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

Make sure there are **no errors** or **warnings** in the output. If there are, please resolve them before submitting your changes.

1. **Push your changes** to your fork and create a pull request to the main repository.

### Code Style Guidelines

- **Indentation**: Use 2 spaces for indentation.
- **Naming conventions**: Follow the existing naming conventions in the codebase.
- **Documentation**: Document all public functions with `roxygen2` comments.
- **Testing**: Ensure that your changes are covered by tests, especially if you're adding new functionality or fixing bugs.



### Running Tests

We use [testthat](https://cran.r-project.org/web/packages/testthat/index.html) for unit testing. To run the tests, use the following command:

```R
devtools::test()
```

Make sure all tests pass before submitting your changes.

### Documentation

If your changes introduce new functionality, make sure to:
- Update the relevant documentation using `roxygen2` comments.
- Update the `README.md` and any relevant vignettes.
- Update the `NEWS.md` with your changes.
- Update the version of the package.

### License

By contributing, you agree that your contributions will be licensed according to [license](LICENSE.md).

