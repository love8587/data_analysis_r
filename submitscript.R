checkPkgs <- function() {
        pkg.inst <- installed.packages()
        pkgs <- c("RCurl", "digest")
        have.pkg <- pkgs %in% rownames(pkg.inst)

        if(any(!have.pkg)) {
                cat("Some packages need to be installed\n")
                r <- readline("Install necessary packages [y/n]? ")
                if(tolower(r) == "y") {
                        need <- pkgs[!have.pkg]
                        message("installing packages ",
                                paste(need, collapse = ", "))
                        install.packages(need)
                }
        }
}

checkPkgs()

CLASS <- "compdata-2012-001"
challenge.url <- paste("http://class.coursera.org", CLASS,
                       "assignment/challenge", sep = "/")
submit.url <- paste("http://class.coursera.org", CLASS,
                       "assignment/submit", sep = "/")

loginPrompt <- function() {
        email <- readline("Submission login (email): ")
        passwd <- readline("Submission  password: ")
        r <- list(email = email, passwd = passwd)
        assign(".CourseraLogin", r, globalenv())
        invisible(r)
}

submit <- function(resetLogin = FALSE) {
        library(RCurl)
        library(digest)
        if(exists(".CourseraLogin") && !resetLogin)
                cred <- get(".CourseraLogin")
        else
                cred <- loginPrompt()
        if(!is.list(cred) || !(names(cred) %in% c("email", "passwd")))
                stop("problem with login/password")
        email <- cred$email
        password <- cred$passwd
        
        ## Prompt Submission Part
        sid <- partPrompt()

        ## Get challenge
        ch <- getChallenge(email)

        ## Get output
        output <- getOutput(sid)
        
        ## Attempt submission with challenge
        ch.resp <- challengeResponse(password, ch$ch.key)
        results <- submitSolution(email, ch.resp, sid, output, ch$state)
        if(results != "Correct!") 
                results <- "Incorrect!"
        cat("Result: ", results, "\n")
        invisible()
}

getOutput <- function(sid) {
        if(sid == "best-1") {
                source("best.R", local = TRUE)
                cat("Running test:\n")
                cat("best(\"SC\", \"heart attack\")\n")
                best("SC", "heart attack")
        }
        else if(sid == "best-2") {
                source("best.R", local = TRUE)
                cat("Running test:\n")
                cat("best(\"NY\", \"pneumonia\")\n")
                best("NY", "pneumonia")
        }
        else if(sid == "best-3") {
                source("best.R", local = TRUE)
                cat("Running test:\n")
                cat("best(\"NN\", \"pneumonia\")\n")
                tryCatch({
                        best("NN", "pneumonia")
                }, error = function(e) {
                        tolower(conditionMessage(e))
                })
        }
        else if(sid == "rankhospital-1") {
                source("rankhospital.R", local = TRUE)
                cat("Running test:\n")
                cat("rankhospital(\"NC\", \"heart attack\", \"worst\")\n")
                rankhospital("NC", "heart attack", "worst")
        }
        else if(sid == "rankhospital-2") {
                source("rankhospital.R", local = TRUE)
                cat("Running test:\n")
                cat("rankhospital(\"WA\", \"heart attack\", 7)\n")
                rankhospital("WA", "heart attack", 7)
        }
        else if(sid == "rankhospital-3") {
                source("rankhospital.R", local = TRUE)
                cat("Running test:\n")
                cat("rankhospital(\"WA\", \"pneumonia\", 1000)\n")
                rankhospital("WA", "pneumonia", 1000)
        }
        else if(sid == "rankhospital-4") {
                source("rankhospital.R", local = TRUE)
                cat("Running test:\n")
                cat("rankhospital(\"NY\", \"heart attak\", 7)\n")
                tryCatch({
                        rankhospital("NY", "heart attak", 7)
                }, error = function(e) {
                        tolower(conditionMessage(e))
                })
        }
        else if(sid == "rankall-1") {
                source("rankall.R", local = TRUE)
                cat("Running test:\n")
                cat("rankall(\"heart attack\", 4)\n")
                r <- rankall("heart attack", 4)
                stopifnot(names(r) %in% c("hospital", "state"))
                as.character(subset(r, state == "HI")$hospital)
        }
        else if(sid == "rankall-2") {
                source("rankall.R", local = TRUE)
                cat("Running test:\n")
                cat("rankall(\"pneumonia\", \"worst\")\n")
                r <- rankall("pneumonia", "worst")
                as.character(subset(r, state == "NJ")$hospital)
        }
        else if(sid == "rankall-3") {
                source("rankall.R", local = TRUE)
                cat("Running test:\n")
                cat("rankall(\"heart failure\", 10)\n")
                r <- rankall("heart failure", 10)
                as.character(subset(r, state == "NV")$hospital)
        }
        else {
                stop("invalid part number")
        }
}

partPrompt <- function() {
        sid <- c("best-1",
                 "best-2",
                 "best-3",
                 "rankhospital-1",
                 "rankhospital-2",
                 "rankhospital-3",
                 "rankhospital-4",
                 "rankall-1",
                 "rankall-2",
                 "rankall-3"
                 )
        ## Just for testing
        ## sid <- paste(sid, "dev", sep = "-")
        sidname <- c("'best' part 1",
                     "'best' part 2",
                     "'best' part 3",
                     "'rankhospital' part 1",
                     "'rankhospital' part 2",
                     "'rankhospital' part 3",
                     "'rankhospital' part 4",
                     "'rankall' part 1",
                     "'rankall' part 2",
                     "'rankall' part 3"
                     )
        numparts <- length(sid)
        cat(paste(paste0("[", seq_len(numparts), "]"), sidname), sep = "\n")
        partnum <- readline(sprintf("Which part are you submitting [1-%d]? ",
                                    numparts))
        partnum <- as.integer(partnum)               
        if(partnum > numparts)
                stop("invalid part number")
        sid[partnum]
}

getChallenge <- function(email) {
        params <- list(email_address = email, response_encoding = "delim")
        result <- getForm(challenge.url, .params = params)
        s <- strsplit(result, "|", fixed = TRUE)[[1]]
        list(ch.key = s[5], state = s[7])
}

challengeResponse <- function(password, ch.key) {
        x <- paste0(ch.key, password)
        digest(x, algo = "sha1", serialize = FALSE)
}

submitSolution <- function(email, ch.resp, sid, output, signature, src = "") {
        output <- as.character(base64(output))
        src <- as.character(base64(src))
        params <- list(assignment_part_sid = sid,
                       email_address = email,
                       submission = output,
                       submission_aux = src,
                       challenge_response = ch.resp,
                       state = signature)
        params <- lapply(params, URLencode)
        result <- postForm(submit.url, .params = params)
        s <- strsplit(result, "\\r\\n")[[1]]
        tail(s, 1)
}
