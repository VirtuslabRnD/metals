package scala.meta.ls

import scala.concurrent.ExecutionContext
import java.util.concurrent.ExecutorService
import scala.concurrent.ExecutionContextExecutorService

trait MetalsThreads {
  def treeViewEc: ExecutionContext
  def dummyBatchedFunctionEc: ExecutionContext
  def dummyEc: ExecutionContext
  def remoteLanguageServer: ExecutionContext
  def shellDestroyProcEc: ExecutionContext
  def pcExecutorService: ExecutorService
  def pcEc: ExecutionContext
  def dbEc: ExecutionContext
  def buildServerJsonRpcEs: ExecutorService
  def buildServerConnStuffEc: ExecutionContext

  def tempEc: ExecutionContext
  def tempEcs: ExecutionContextExecutorService
}

object MetalsThreads {
  def apply(ec: ExecutionContextExecutorService): MetalsThreads =
    new MetalsThreads {
      def treeViewEc = ec
      def dummyBatchedFunctionEc = ec
      def dummyEc = ec
      def remoteLanguageServer = ec
      def shellDestroyProcEc = ec
      def pcExecutorService = ec
      def pcEc = ec
      def dbEc = ec
      def buildServerJsonRpcEs = ec
      def buildServerConnStuffEc = ec

      def tempEc = ec
      def tempEcs = ec
    }
}
