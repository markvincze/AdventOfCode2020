let subjectNumber = 7L
let publicKey1 = 11562782L
let publicKey2 = 18108497L
// let publicKey1 = 5764801L
// let publicKey2 = 17807724L

let findLoopSize subjectNumber publicKey =
    let rec findLoopSize subjectNumber publicKey current iter =
        let value = current * subjectNumber % 20201227L
        if value = publicKey
        then iter
        else findLoopSize subjectNumber publicKey value (iter + 1L)
    
    findLoopSize subjectNumber publicKey 1L 1L

let rec transform subjectNumber loopSize value =
    if loopSize = 0L
    then value
    else transform subjectNumber (loopSize - 1L) (value * subjectNumber % 20201227L)

let secretNum1 = findLoopSize subjectNumber publicKey1
let secretNum2 = findLoopSize subjectNumber publicKey2

let encryptionKey1 = transform publicKey2 secretNum1 1L
let encryptionKey2 = transform publicKey1 secretNum2 1L
