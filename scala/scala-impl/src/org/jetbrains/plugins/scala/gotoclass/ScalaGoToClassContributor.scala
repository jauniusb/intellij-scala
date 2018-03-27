package org.jetbrains.plugins.scala
package gotoclass

import com.intellij.navigation.{ChooseByNameContributor, NavigationItem}
import com.intellij.openapi.project.Project
import com.intellij.psi.search.GlobalSearchScope
import org.jetbrains.plugins.scala.lang.psi.stubs.index.ScalaIndexKeys
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil

/**
 * Nikolay.Tropin
 * 12/19/13
 */
class ScalaGoToClassContributor extends ChooseByNameContributor {

  import ScalaIndexKeys._

  def getNames(project: Project, includeNonProjectItems: Boolean): Array[String] = {
    implicit val projectCopy: Project = project

    Array(NOT_VISIBLE_IN_JAVA_SHORT_NAME_KEY, PACKAGE_OBJECT_SHORT_NAME_KEY).flatMap(_.keys)
  }

  def getItemsByName(name: String, pattern: String, project: Project, includeNonProjectItems: Boolean): Array[NavigationItem] = {
    val scope = if (includeNonProjectItems) GlobalSearchScope.allScope(project) else GlobalSearchScope.projectScope(project)
    val cleanName = ScalaNamesUtil.cleanFqn(name)

    implicit val projectCopy: Project = project
    Array(NOT_VISIBLE_IN_JAVA_SHORT_NAME_KEY, PACKAGE_OBJECT_SHORT_NAME_KEY).flatMap(_.elements(cleanName, scope))
  }
}
