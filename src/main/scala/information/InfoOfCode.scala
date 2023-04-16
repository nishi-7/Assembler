package information

class Info {
  private var numOfACmd = 0
  private var numOfACmdLabel = 0
  private var numOfCCmd = 0
  private var numOfLabel = 0

  def doInfoReport(): String = {
    s"[Info] number of ACmd      = ${numOfACmd}\n" +
      s"[Info] number of ACmdLabel = ${numOfACmdLabel}\n" +
      s"[Info] number of CCmd      = ${numOfCCmd}\n" +
      s"[Info] number of Label     = ${numOfLabel}"
  }

  def setNumOfACmd(state: Int) = {
    numOfACmd = state
  }

  def addNumOfACmd(state: Int) = {
    numOfACmd += state
  }

  def setNumOfACmdLabel(state: Int) = {
    numOfACmdLabel = state
  }

  def addNumOfACmdLabel(state: Int) = {
    numOfACmdLabel += state
  }

  def setNumOfCCmd(state: Int) = {
    numOfCCmd = state
  }

  def addNumOfCCmd(state: Int) = {
    numOfCCmd += state
  }

  def setNumOfLabel(state: Int) = {
    numOfLabel = state
  }

  def addNumOfLabel(state: Int) = {
    numOfLabel += state
  }

}
