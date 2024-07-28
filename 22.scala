import scala.io.StdIn.readLine

object StudentRecord extends App {

  case class StudentRecord(name: String, marks: Int, totalMarks: Int, percentage: Double, grade: Char)

  def getStudentInfo(): (String, Int, Int, Double, Char) = {
    val name = readLine("Enter student's name: ")
    val marks = readLine("Enter student's marks: ").toInt
    val totalMarks = readLine("Enter total possible marks: ").toInt

    validateInput(name, marks, totalMarks) match {
      case (true, _) =>
        val percentage = (marks.toDouble / totalMarks) * 100
        val grade = percentage match {
          case p if p >= 90 => 'A'
          case p if p >= 75 => 'B'
          case p if p >= 50 => 'C'
          case _ => 'D'
        }
        (name, marks, totalMarks, percentage, grade)
      case (false, Some(errorMessage)) =>
        println(s"Error: $errorMessage")
        getStudentInfo() 
      case (false, None) =>
        getStudentInfo() 
    }
  }

  def printStudentRecord(student: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = student
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks")
    println(s"Total Possible Marks: $totalMarks")
    println(s"Percentage: ${"%.2f".format(percentage)}%")
    println(s"Grade: $grade")
  }

  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {

    if (name.isEmpty) {
      (false, Some("Name cannot be empty"))
    } else if (marks < 0 || marks > totalMarks) {
      (false, Some("Invalid marks: Marks must be between 0 and totalMarks"))
    } else if (totalMarks <= 0) {
      (false, Some("Invalid totalMarks: Total marks must be greater than 0"))
    } else {
      (true, None)
    }
  }

  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var isValid = false
    var studentRecord: (String, Int, Int, Double, Char) = null

    while (!isValid) {
      studentRecord = getStudentInfo()
      isValid = studentRecord._1.nonEmpty 
    }

    studentRecord
  }

  val student = getStudentInfoWithRetry()
  printStudentRecord(student)

}
